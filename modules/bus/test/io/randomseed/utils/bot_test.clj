(ns io.randomseed.utils.bot-test

  (:require [clojure.test :refer [deftest is testing]]
            [io.randomseed.utils.bot :as bot]
            [io.randomseed.utils.bus :as bus]))

(deftest bot-ns-always-a-symbol
  (testing "qualified keyword -> symbol namespace+name"
    (is (= 'my.custom.bot (bot/bot-ns :my.custom/bot :fallback))))

  (testing "string -> symbol"
    (is (= 'my.custom.ns (bot/bot-ns "my.custom.ns" :fallback))))

  (testing "map with :ns already a symbol"
    (is (= 'my.custom.ns (bot/bot-ns 'my.custom.ns :fallback))))

  (testing "nil falls back to id"
    (is (= 'my.custom.bot (bot/bot-ns nil :my.custom/bot)))))

(deftest bot?-shape
  (testing "bot? is a pure shape predicate"
    (is (true? (bot/bot? {:bot :x :ns 'x})))
    (is (false? (bot/bot? {:bot nil :ns 'x})))
    (is (false? (bot/bot? {:bot :x})))
    (is (false? (bot/bot? nil)))))

(deftest instance-config-validation
  (testing "instances present without instance-id is rejected"
    (is (thrown? AssertionError
                 (bot/instance-config {:ns        'a.b
                                       :bot       :x
                                       :instances {:one {:x 1}}}
                                      nil))))

  (testing "instance-id must point to a non-empty instance"
    (is (thrown? AssertionError
                 (bot/instance-config {:ns        'a.b
                                       :bot       :x
                                       :instances {:one nil}}
                                      :one))))

  (testing "valid instance-id adds :instance and merges instance config"
    (let [cfg (bot/instance-config {:ns        'a.b
                                    :bot       :x
                                    :instances {:one {:x 1}}}
                                   :one)]
      (is (= :a.b/one (:instance cfg)))
      (is (= 1 (:x cfg))))))

(deftest parse-config-minimal
  (testing "parse-config returns nil when :bot is missing"
    (is (nil? (bot/parse-config {}))))

  (testing "parse-config fills derived keys and can instantiate when :instances is present"
    (let [cfg (bot/parse-config {:bot       :gamma
                                 :ns        :my.custom/ns
                                 :instances {:one {:x 1}}}
                                :one)]
      (is (= :io.randomseed.utils.bot/gamma (:bot cfg)))      (is (symbol? (:ns cfg)))
      (is (= :my.custom.ns/one (:instance cfg)))
      (is (= 1 (:x cfg)))
      (is (nil? (:instances cfg))) ; bo instance-config robi (dissoc :instances)
      (is (bot/bot? cfg)))))

(deftest builtin-command-wrappers-use-simple-keywords
  (let [calls (atom [])]
    (with-redefs [bus/request->response
                  (fn [wrk req & args]
                    (swap! calls conj [wrk req (vec args)])
                    {:body :ok})]
      (is (= :ok (bot/get-config :w1)))
      (is (= :ok (bot/get-session :w2)))
      (is (= :ok (bot/pause :w3)))
      (is (= :ok (bot/ping :w4))))
    (is (= [[:w1 :config []]
            [:w2 :session []]
            [:w3 :pause []]
            [:w4 :ping []]]
           @calls))))

(deftest stop-bang-uses-simple-stop-command
  (let [new-req-calls (atom [])
        stop-calls    (atom [])]
    (with-redefs [bus/worker      identity
                  bus/new-request (fn [wrk req]
                                    (swap! new-req-calls conj [wrk req])
                                    (bus/->Request 1 wrk req []))
                  bus/stop-worker (fn [wrk msg]
                                    (swap! stop-calls conj [wrk msg])
                                    :stopped)]
      (is (true? (bot/stop! :w1))))
    (is (= [[:w1 :stop]] @new-req-calls))
    (is (= 1 (count @stop-calls)))))

(deftest update-local-config-uses-simple-config-command
  (let [request-calls (atom [])
        update-calls  (atom [])]
    (with-redefs [bus/request->response (fn [wid req & args]
                                          (swap! request-calls conj [wid req (vec args)])
                                          (bus/->Response 1 wid {:x 1} nil))
                  bus/update-config!    (fn [wid cfg]
                                          (swap! update-calls conj [wid cfg])
                                          :updated)]
      (bot/update-local-config! :worker-1))
    (is (= [[:worker-1 :config []]] @request-calls))
    (is (= [[:worker-1 {:x 1}]] @update-calls))))

(deftest qualified-commands-are-delegated-to-custom-handler
  (let [handled (atom nil)
        wrk     (bus/->Worker :worker nil nil nil nil {:cfg true})
        req     (bus/->Request 1 :worker ::extended [])
        reply   (bot/generic-control {:stage :RUNNING}
                                     (fn [_ _ r _]
                                       (reset! handled (:body r))
                                       :handled)
                                     wrk req nil)]
    (is (= ::extended @handled))
    (is (= :handled (:body reply)))))
