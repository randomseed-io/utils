(ns

    ^{:doc    "Random utils, inter-thread bus."
      :author "Paweł Wilk"
      :added  "1.0.0"}

    io.randomseed.utils.bus

  (:refer-clojure :exclude [load run])

  (:require [clojure.set                     :as               set]
            [clojure.string                  :as               str]
            [io.randomseed.utils             :refer           :all]
            [io.randomseed.utils.fs          :as                fs]
            [io.randomseed.utils.map         :as               map]
            [io.randomseed.utils.log         :as               log]
            [io.randomseed.utils.time        :as              time]
            [tick.core                       :as                 t]
            [clojure.core.async              :as             async]
            [clojure.core.async              :refer [<! <!! >! >!!
                                                     alts! alts!!
                                                     chan close!
                                                     go go-loop
                                                     put! thread]]))

(def current-ns-str (str (ns-name *ns*)))

;;
;; records
;;

(defrecord Worker   [id fn control data result config])
(defrecord Workers  [ids db])
(defrecord Request  [id to body args])
(defrecord Response [id from body request])
(defrecord Reply    [body data])
(defrecord Outcome  [request response data])

(defn worker?   [v] (instance? Worker   v))
(defn workers?  [v] (instance? Workers  v))
(defn request?  [v] (instance? Request  v))
(defn response? [v] (instance? Response v))
(defn reply?    [v] (instance? Reply    v))
(defn outcome?  [v] (instance? Outcome  v))

;;
;; shared workers data
;;

(defonce workers (atom (Workers. {} {})))

;;
;; channels
;;

(defn new-control-channel [] (chan 24))
(defn new-data-channel    [] (chan 24))

;;
;; getting worker from record or shared workers database
;;

(defn worker
  ([v]
   (worker @workers v))
  ([workers v]
   (when v
     (if (worker? v) v (get (.db workers) v)))))

(defn worker-id
  [v]
  (if (worker? v) (.id ^Worker v) v))

(defn worker-exists?
  ([v]
   (worker-exists? @workers v))
  ([workers v]
   (when v
     (contains? (.db ^Workers workers)
                (if (worker? v) (.id ^Worker v) v)))))

(defn list-workers
  ([]
   (list-workers workers))
  ([workers]
   (when workers
     (keys (.db ^Workers workers)))))

(defn config
  ([wid]
   (config @workers wid))
  ([workers wid]
   (assert (ident? wid) (str "Worker ID required but " wid " given."))
   (when-some [wrk (worker workers wid)]
     (.config ^Worker wrk))))

;;
;; closing worker channels
;;

(defn close
  [wrk]
  (when-some [wrk (worker wrk)]
    (close! (.control ^Worker wrk))
    (close! (.data    ^Worker wrk))))

;;
;; basic operations on workers
;;

(defn control-channel [wrk]     (.control ^Worker (worker wrk)))
(defn data-channel    [wrk]     (.data    ^Worker (worker wrk)))

(defn put-command      [wrk msg] (>!! (control-channel wrk) msg))
(defn put-data         [wrk msg] (>!! (data-channel    wrk) msg))
(defn wait-for-command [wrk]     (<!! (control-channel wrk)))
(defn wait-for-data    [wrk]     (<!! (data-channel    wrk)))
(defn wait-for-result  [wrk]     (<!! (.result ^Worker (worker wrk))))

(defn try-put-command  [wrk msg] (async/offer! (control-channel wrk) msg))
(defn try-put-data     [wrk msg] (async/offer! (data-channel    wrk) msg))
(defn get-command      [wrk]     (async/poll!  (control-channel wrk)))
(defn get-data         [wrk]     (async/poll!  (data-channel    wrk)))
(defn get-result       [wrk]     (async/poll!  (.result ^Worker (worker wrk))))

(defn handle-command   [wrk f]   (async/take! (control-channel wrk) f))

;;
;; request and response constructors
;;

(defn new-request
  ([wrk req]
   (new-request wrk req nil))
  ([wrk req id & args]
   (if (request? req)
     (if id
       (Request. (or id (.id ^Request req) (time/timestamp))
                 (.id   ^Worker wrk)
                 (.body ^Request req)
                 (.args ^Request args))
       req)
     (Request. (or id (time/timestamp))
               (.id ^Worker wrk)
               req
               args))))

(defn new-response
  ([wrk res]
   (new-response wrk res nil nil))
  ([wrk res req]
   (new-response wrk res req nil))
  ([wrk res req id]
   (if (response? res)
     (if (or id req)
       (Response. (or id (.id ^Response res) (time/timestamp))
                  (.id   ^Worker wrk)
                  (.body ^Response res)
                  (or req (.request ^Response res)))
       res)
     (Response. (or id (time/timestamp))
                (.id ^Worker wrk)
                res
                req))))

;;
;; request and response basic operations
;;

(defn wait-for-request  [wrk] (wait-for-command  wrk))
(defn wait-for-response [wrk] (wait-for-data     wrk))
(defn receive-request   [wrk] (get-command       wrk))
(defn receive-response  [wrk] (get-data          wrk))

(defn send-request
  ([wrk req]
   (let [wrk (worker wrk)
         r   (new-request wrk req)]
     (when (put-command wrk r) r)))
  ([wrk req & args]
   (let [wrk (worker wrk)
         r   (apply new-request wrk req nil args)]
     (when (put-command wrk r) r))))

(defn send-id-request
  ([wrk req id]
   (let [wrk (worker wrk)
         r   (new-request wrk req id)]
     (when (put-command wrk r) r)))
  ([wrk req id & args]
   (let [wrk (worker wrk)
         r   (apply new-request wrk req id args)]
     (when (put-command wrk r) r))))

(defn try-send-request
  ([wrk req]
   (let [wrk (worker wrk)
         r   (new-request wrk req)]
     (when (try-put-command wrk r) r)))
  ([wrk req & args]
   (let [wrk (worker wrk)
         r   (apply new-request wrk req nil args)]
     (when (try-put-command wrk r) r))))

(defn try-send-id-request
  ([wrk req id]
   (let [wrk (worker wrk)
         r   (new-request wrk req id)]
     (when (try-put-command wrk r) r)))
  ([wrk req id & args]
   (let [wrk (worker wrk)
         r   (apply new-request wrk req id args)]
     (when (try-put-command wrk r) r))))

(defn send-response
  ([wrk res]
   (let [wrk (worker wrk)
         r   (new-response wrk res)]
     (when (put-data wrk r) r)))
  ([wrk res req]
   (let [wrk (worker wrk)
         r   (new-response wrk res req)]
     (when (put-data wrk r) r)))
  ([wrk res req id & args]
   (let [wrk (worker wrk)
         r   (new-response wrk res req id)]
     (when (put-data wrk r) r))))

(defn try-send-response
  ([wrk res]
   (let [wrk (worker wrk)
         r   (new-response wrk res)]
     (when (try-put-data wrk r) r)))
  ([wrk res req]
   (let [wrk (worker wrk)
         r   (new-response wrk res req)]
     (when (try-put-data wrk r) r)))
  ([wrk res req id & args]
   (let [wrk (worker wrk)
         r   (new-response wrk res req id)]
     (when (try-put-data wrk r) r))))

;;
;; complex operations on requests and responses
;;

(def ^:const empty-reply
  (Reply. nil nil))

(defn new-reply
  "Used to enrich the output of a handling function so it can return both: response
  body and additional data."
  ([]
   empty-reply)
  ([body]
   (if (nil? body)
     empty-reply
     (if (reply? body)
       body
       (Reply. body nil))))
  ([body data]
   (if (and (nil? body) (nil? data))
     empty-reply
     (Reply. (if (reply? body) (.body ^Reply body) body)
             data))))

(def ^:const empty-outcome
  (Outcome. nil nil nil))

(defn new-outcome
  "Used to wrap the outcome of request handling and to convert the output to the Outcome
  object."
  ([]
   empty-outcome)
  ([req]
   (if (nil? req)
     empty-outcome
     (if (outcome? req) req (Outcome. req nil nil))))
  ([req res]
   (if (and (nil? req) (nil? res))
     empty-outcome
     (if (outcome? req)
       (Outcome. (.request ^Outcome req) res (.data ^Outcome req))
       (Outcome. req res nil))))
  ([req res data]
   (if (and (nil? req) (nil? res) (nil? data))
     empty-outcome
     (if (outcome? req)
       (Outcome. (.request ^Outcome req) res data)
       (Outcome. req res data)))))

(defn request->response
  "Sends a blocking request and waits for any response."
  ([wrk req]
   (let [wrk (worker wrk)]
     (when (send-request wrk req)
       (wait-for-response wrk))))
  ([wrk req & args]
   (let [wrk (worker wrk)]
     (when (apply send-request wrk req args)
       (wait-for-response wrk)))))

(defn id-request->response
  "Sends a blocking request and waits for any response."
  ([wrk req id]
   (let [wrk (worker wrk)]
     (when (send-request wrk req id)
       (wait-for-response wrk))))
  ([wrk req id & args]
   (let [wrk (worker wrk)]
     (when (apply send-request wrk req id args)
       (wait-for-response wrk)))))

(defn handle-request
  [wrk req f & handler-args]
  (if (nil? req)
    empty-outcome
    (if-not (request? req)
      (do (log/warn "Ignoring unknown control message / not a request object")
          empty-outcome)
      (if-some [r (f wrk req handler-args)]
        (do
          (if (reply? r)
            (if (= ::no-response (.body ^Reply r))
              (do (log/trace "Handler requested to not send the response, omitting")
                  (new-outcome req nil (.data ^Reply r)))
              (do (log/trace "Sending response")
                  (new-outcome req (new-response wrk (.body ^Reply r) req) (.data ^Reply r))))
            (if (= ::no-response r)
              (do (log/trace "Handler requested to not send the response, omitting")
                  (new-outcome req))
              (do (log/trace "Sending response")
                  (new-outcome req (new-response wrk r req))))))
        (do (log/debug "Handler returned nil, omitting response")
            empty-outcome)))))

(defn- process-request-core
  "Receives a request from the control channel and calls a handler f on worker object,
  request and args provided.

  If the handler function returns nil or :kara.bus/no-response, the response should
  not be created and the returned value is an empty Outcome object.

  If the handler returns a Reply object, the response should be generated even if
  its :body field is nil. This is helpful in cases where nil responses are required.

  If the handler returns a Reply object but its :body is set to :kara.bus/no-response
  then no response will be generated and the response in the returned Outcome object
  will be set to nil.

  Otherwise an Outcome object will be returned with :request, :response and :data
  fields."
  [wrk req f args]
  (if-some [r (apply f wrk req args)]
    (if (reply? r)
      (if (= ::no-response (.body ^Reply r))
        (do (log/trace "Handler requested to not send the response, omitting")
            (new-outcome req nil (.data ^Reply r)))
        (do (log/trace "Sending response")
            (new-outcome req (send-response wrk (.body ^Reply r) req)) (.data ^Reply r)))
      (if (= ::no-response r)
        (do (log/trace "Handler requested to not send the response, omitting")
            (new-outcome req))
        (do (log/trace "Sending response")
            (new-outcome req (send-response wrk r req)))))
    (do (log/trace "Handler returned nil, omitting response")
        empty-outcome)))

(defn try-process-request
  "Receives a request from the control channel and calls a handler f on worker object,
  request and args provided. Does not block on receiving (returns immediately when
  there is no data on the control channel and blocks on sending (waits for the
  channel to have some space for a new message).

  If the handler function returns nil or :kara.bus/no-response, the response should
  not be created and the returned value is an empty Outcome object.

  If the handler returns a Reply object, the response should be generated even if
  its :body field is nil. This is helpful in cases where nil responses are required.

  If the handler returns a Reply object but its :body is set to :kara.bus/no-response
  then no response will be generated and the response in the returned Outcome object
  will be set to nil.

  Otherwise an Outcome object will be returned with :request, :response and :data
  fields."
  [wrk f & args]
  (log/trace "Checking request")
  (if-some [req (receive-request wrk)]
    (do (log/trace (str "Request received: " req))
        (process-request-core wrk req f args))
    (do (log/trace "No request yet")
        empty-outcome)))

(defn process-request
  "Receives a request from the control channel and calls a handler f on worker object,
  request and args provided. Blocks on receiving (waits for data) and on
  sending (waits for the channel to have some space for a new message).

  If the handler function returns nil or :kara.bus/no-response, the response should
  not be created and the returned value is an empty Outcome object.

  If the handler returns a Reply object, the response should be generated even if
  its :body field is nil. This is helpful in cases where nil responses are required.

  If the handler returns a Reply object but its :body is set to :kara.bus/no-response
  then no response will be generated and the response in the returned Outcome object
  will be set to nil.

  Otherwise an Outcome object will be returned with :request, :response and :data
  fields."
  [wrk f & args]
  (log/trace "Waiting for request")
  (let [req (wait-for-request wrk)]
    (log/trace (str "Request received: " req))
    (process-request-core wrk req f args)))

;;
;; workers construction and registering
;;

(defn new-worker-with-wid
  ([w id config f multi?]
   (let [id  (ensure-ident-keyword id)
         num (when multi? (inc (get (.ids w) id -1)))
         nst (when multi? (str "-" num))
         wid (keyword (namespace id) (str (name id) nst))]
     [(when-not (contains? (.db ^Workers w) wid)
        (let [wrk (Worker. wid f (new-control-channel) (new-data-channel) nil config)]
          (Workers. (assoc (.ids ^Workers w)  id num)
                    (assoc (.db  ^Workers w) wid wrk))))
      wid])))

(defn new-worker
  ([w id config f]
   (new-worker w id config false))
  ([w id config f multi?]
   (nth (new-worker-with-wid w id config f multi?) 0)))

(defn new-worker!
  ([id config f]
   (new-worker! workers id config f))
  ([workers id config f]
   (let [mlt (or (and (:multiple config) true) false)]
     (loop []
       (let [old @workers
             new (new-worker-with-wid old id config f mlt)
             wrs (nth new 0)
             wid (nth new 1)]
         (if-not (workers? wrs)
           (if mlt
             (recur)
             (throw (ex-info (str "Worker " wid " is already running.")
                             {:worker-id wid :worker-exists true})))
           (if (compare-and-set! workers old wrs)
             wid
             (recur))))))))

(defn update-worker
  ([workers wrk]
   (Workers. (.ids ^Workers workers)
             (assoc (.db ^Workers workers) (worker-id wrk) wrk)))
  ([workers wrk f & args]
   (Workers. (.ids ^Workers workers)
             (apply update (.db ^Workers workers) (worker-id wrk) f args))))

(defn update-worker!
  [& args]
  (let [[workers wrk f & args] (if (instance? clojure.lang.Atom (first args))
                                 args (cons workers args))]
    (apply swap!
           workers
           update-worker
           (worker-id wrk)
           (or f (constantly (worker wrk)))
           args)))

(defn update-config
  [workers wrk f & args]
  (when-some [wrk (worker workers wrk)]
    (let [db   (.db ^Workers workers)
          wid  (worker-id wrk)
          orig (get db wid)]
      (Workers. (.ids ^Workers workers)
                (assoc db wid
                       (Worker. (.id      ^Worker orig)
                                (.fn      ^Worker orig)
                                (.control ^Worker orig)
                                (.data    ^Worker orig)
                                (.result  ^Worker orig)
                                (if (fn? f)
                                  (apply f (.config  ^Worker orig) args)
                                  f)))))))

(defn update-config!
  [& args]
  (let [[workers wrk f & args] (if (instance? clojure.lang.Atom (first args))
                                 args (cons workers args))]
    (apply swap!
           workers
           update-config
           (worker-id wrk)
           f args)))

(defn remove-worker
  [workers wrk]
  (Workers. (.ids workers)
            (dissoc (.db ^Workers workers) (worker-id wrk))))

(defn remove-worker!
  ([wrk]
   (swap! workers remove-worker wrk))
  ([wrokers wrk]
   (swap! workers remove-worker wrk)))

;;
;; worker starting and stopping
;;

(defn start-worker
  [id config f & args]
  (let [wid (new-worker! id config f)]
    (when-not (worker-exists? @workers wid)
      (throw (ex-info (str "Worker " wid " could not be added.") {:worker-id wid})))
    (worker
     (update-worker!
      wid
      (fn [w]
        (let [fun (.fn ^Worker w)
              ctx {:worker-id wid}
              ctx (when-some [n (:name config)] (assoc ctx :module n))
              res (thread
                    (log/with-ctx ctx
                      (log/debug (str "Calling runner: " (fn-name f)))
                      (log/log-exceptions (apply fun w args))))]
          (Worker. wid
                   (.fn      ^Worker w)
                   (.control ^Worker w)
                   (.data    ^Worker w)
                   res
                   (.config  ^Worker w)))))
     wid)))

(defn stop-worker
  ([wrk]
   (stop-worker wrk nil))
  ([wrk msg-or-fn]
   (when-some [wrk (worker wrk)]
     (let [wid (.id ^Worker wrk)]
       (when msg-or-fn
         (log/info (str "Shutting down worker: " (symbol wid)))
         (if (fn? msg-or-fn)
           (let [nam (fn-name msg-or-fn)]
             (log/debug (str "Calling shutdown function: " nam))
             (msg-or-fn wrk))
           (do (log/trace (str "Sending control message: " msg-or-fn))
               (send-request wrk msg-or-fn))))
       (Thread/sleep 1000)
       (log/trace "Closing worker channels")
       (close wrk)
       (log/debug (str "Removing worker from pool: " (symbol wid)))
       (remove-worker! wid)
       (Thread/sleep 1000)
       wid))))
