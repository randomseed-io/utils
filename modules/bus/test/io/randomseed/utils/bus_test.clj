(ns io.randomseed.utils.bus-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.core.async :as async :refer [<!!]]
            [clojure.test.check.clojure-test :refer [defspec]]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]
            [io.randomseed.utils.bus :as bus]))

;; IMPORTANT
;; ---------
;; These tests must reference ONLY vars that exist in `io.randomseed.utils.bus`
;; (per project rules). If we need a predicate/helper, we define it locally.

(defn- mk-worker
  "Creates a minimal worker with fresh channels (no background threads).

  Uses bus/new-worker-with-wid which returns [workers' wid], then extracts
  the Worker record and attaches a buffered :result channel for tests."
  ([] (mk-worker :w {} (fn [& _] nil) false))
  ([id] (mk-worker id {} (fn [& _] nil) false))
  ([id config f multi?]
   (let [w0       (bus/->Workers {} {})
         [w1 wid] (bus/new-worker-with-wid w0 id config f multi?)
         wrk      (bus/worker w1 wid)]
     ;; new-worker-with-wid sets result=nil; tests often want it present
     (assoc wrk :result (async/chan 8)))))

(defn- msg-id
  "Extracts the id from any of the message records used in bus.clj."
  [m]
  (:id m))

(deftest predicates-smoke
  (testing "record predicates"
    (let [wrk (mk-worker)
          req (bus/new-request wrk :ping [1 2])
          res (bus/new-response wrk :pong req)
          rep (bus/new-reply :ok {:a 1})
          out (bus/new-outcome req res {:x true})]
      (is (bus/worker? wrk))
      (is (bus/request? req))
      (is (bus/response? res))
      (is (bus/reply? rep))
      (is (bus/outcome? out))
      (is (not (bus/worker? req)))
      (is (not (bus/request? wrk)))
      (is (not (bus/response? req))))))

(deftest send-request-enqueues-and-returns-request
  (let [wrk (mk-worker :worker-1)
        req (bus/send-request wrk :ping 1 2 3)
        got (<!! (bus/control-channel wrk))]
    (is (bus/request? req))
    (is (= :worker-1 (:to req)))
    (is (= :ping (:body req)))
    (is (= [1 2 3] (:args req)))
    ;; IDs are generated via `time/timestamp` in this codebase (a number),
    ;; not necessarily UUIDs.
    (is (number? (msg-id req)))
    (is (= req got))))

(deftest send-response-enqueues-and-returns-response
  (let [wrk (mk-worker :worker-2)
        req (bus/new-request wrk :ping [])
        rep (bus/new-reply :pong {:a 1})
        res (bus/send-response wrk rep req)
        got (<!! (bus/data-channel wrk))]
    (is (bus/response? res))
    (is (= :worker-2 (:from res)))
    (is (= rep (:body res)))
    (is (= req (:request res)))
    (is (number? (msg-id res)))
    (is (= res got))))

(deftest request->response-roundtrip
  (let [wrk (mk-worker :worker-3)
        ;; Simulate the worker loop: take request from control channel and reply.
        _   (future
              (let [req (<!! (bus/control-channel wrk))
                    res (bus/new-response wrk :pong req)]
                (bus/put-data wrk res)))
        res (bus/request->response wrk :ping 1 2)]
    (is (bus/response? res))
    (is (= :pong (:body res)))
    (is (= :ping (-> res :request :body)))
    (is (= [1 2] (-> res :request :args)))))

;; --- Generative / property tests -------------------------------------------

(defspec new-reply-idempotent 200
  (prop/for-all [body (gen/one-of [gen/keyword
                                   gen/string-alphanumeric
                                   gen/int
                                   gen/boolean
                                   (gen/return nil)])
                 data (gen/one-of [(gen/map gen/keyword gen/int)
                                   (gen/vector gen/int)
                                   (gen/return nil)])]
                (let [r1 (bus/new-reply body data)
                      r2 (bus/new-reply r1)]
                  (= r1 r2))))

(defspec new-outcome-idempotent 200
  (prop/for-all [req-body (gen/one-of [gen/keyword gen/string-alphanumeric gen/int])
                 res-body (gen/one-of [gen/keyword gen/string-alphanumeric gen/int])
                 data     (gen/one-of [(gen/map gen/keyword gen/int)
                                       (gen/vector gen/int)
                                       (gen/return nil)])]
                (let [wrk (mk-worker :w)
                      req (bus/new-request  wrk req-body 1)
                      res (bus/new-response wrk res-body req)
                      o1  (bus/new-outcome req res data)
                      o2  (bus/new-outcome o1)]
                  (= o1 o2))))

