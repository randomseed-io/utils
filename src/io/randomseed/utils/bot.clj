(ns

    ^{:doc    "Random utils, bot abstraction."
      :author "Paweł Wilk"
      :added  "1.0.0"}

    io.randomseed.utils.bot

  (:refer-clojure :exclude [load run! stop! get parse-long uuid random-uuid])

  (:require [clojure.set                     :as             set]
            [clojure.string                  :as             str]
            [io.randomseed.utils             :refer         :all]
            [io.randomseed.utils.bus         :as             bus]
            [io.randomseed.utils.log         :as             log])

  (:import [io.randomseed.utils.bus Worker Workers Response Request Reply Outcome]))

(def current-ns-str
  (str (ns-name *ns*)))

(defn load-config
  ([cfg-parser bot-id]
   (cfg-parser :bot bot-id))
  ([cfg-parser config-file bot-id]
   (cfg-parser config-file :bot bot-id)))

(defn bot-ns
  [v id]
  (if (valuable? v)
    (if (qualified-ident? v)
      (symbol (str (namespace v) "." (name v)))
      (if-some [v (some-str v)] (symbol v)))
    (symbol (str (namespace id) "." (name id)))))

(defn instance-config
  "Creates an instance of a configuration for the given instance (if a bot is
  instantiable)."
  [cfg instance-id]
  (assert (or (nil? instance-id) (valuable? (clojure.core/get (:instances cfg) instance-id)))
          (str "Instance configuration is not present: " instance-id))
  (assert (not (and (nil? instance-id) (valuable? (:instances cfg))))
          (str "This bot requires instantiating but no instance ID was given: " (:name cfg)))
  (if (nil? instance-id)
    cfg
    (let [icfg  (clojure.core/get (:instances cfg) instance-id)
          iname (some-str (:name icfg))]
      (-> cfg
          (dissoc :instances)
          (merge  icfg)
          (assoc  :name (or iname (str (:name cfg) " (" (some-str instance-id) ")"))
                  :instance (keyword (some-str (:ns cfg)) (some-str-simple instance-id)))))))

(defn parse-config
  ([cfg]
   (parse-config cfg nil))
  ([cfg instance-id]
   (if-some [id (clojure.core/get cfg :bot)]
     (let [id (ensure-ns id current-ns-str)]
       (-> cfg
           (assoc  :bot  id)
           (update :ns   bot-ns id)
           (update :name some-str)
           (instance-config instance-id))))))

(defn bot?
  [v]
  (and (map? v)
       (keyword? (:bot v))
       (symbol?  (:ns  v))))

(defn load
  ([config-parser id]
   (load id nil))
  ([config-parser id instance-id]
   (if-some [cfg (parse-config (load-config id) instance-id)]
     (let [[bid cid nsc] (map cfg [:bot :id :ns])]
       (if nsc (try-require nsc))
       (if (qualified-ident?  id) (try-require (namespace  id)))
       (if (qualified-ident? bid) (try-require (namespace bid)))
       (if (qualified-ident? cid) (try-require (namespace cid)))
       cfg))))

(declare command)
(declare update-local-config!)

(defn run!
  "Runs a bot by creating a worker thread with communication channels. After bot is
  successfully created it queries it for its updated configuration and replaces a local
  one with it."
  ([wrk]
   (command wrk :run) true)
  ([bot-session cfg]
   (run! bot-session cfg nil))
  ([bot-session cfg instance]
   (assert (or (keyword? cfg) (bot? cfg)) "Second argument must be a bot configuration map or a bot ID.")
   (when cfg
     (if (or (bus/worker? cfg) (bus/worker-exists? cfg))
       nil
       (let [cfg (if (bot? cfg) cfg (load cfg instance))
             bid (or (:instance cfg) (:id cfg) (:bot cfg))
             mlt (or (and (:multiple cfg) true) false)
             nam (or (:name cfg) bid)
             nam (if (qualified-ident? nam) (name nam) (str nam))]
         (log/with-ctx {:module nam}
           (when-not bot-session
             (throw (ex-info (str "Session is not initialized for the bot " nam) cfg)))
           (let [sid (:sid bot-session)
                 sid (if sid (str " in session " (some-str sid)))
                 fnc (:fn cfg)
                 fnc (if (fn? fnc) fnc
                         (if (qualified-ident? fnc)
                           (resolve (symbol fnc))
                           (if-some [n (:ns cfg)] (ns-resolve n (symbol (or fnc 'run!))))))
                 cfg (assoc cfg :fn fnc :name nam :multiple mlt)]
             (log/info (str "Starting bot " nam sid))
             (when-some [wrk (bus/start-worker bid cfg fnc bot-session)]
               (Thread/sleep 1000)
               (or (valuable
                    (clojure.core/get (.db ^Workers (update-local-config! wrk))
                                      (bus/worker-id wrk)))
                   wrk)))))))))

;;
;; generic request handler
;;

(defn generic-control
  "Common tasks which any bot should support."
  [bot-session f wrk req handler-args]
  (let [args (.args ^Request req)]
    (bus/new-reply
     (case (.body ^Request req)

       :config (do (log/debug "Worker config request")
                   (.config ^Worker wrk))

       :session (do (log/debug "Session request")
                    bot-session)

       :ping (do (log/debug "Ping request")
                 :pong)

       :pause (do (log/info "Pause request")
                  (bus/new-reply :paused :PAUSE))

       :run (do (log/info "Un-pause request")
                (bus/new-reply :unpaused :RUN))

       :stop (do (log/info "Stop request received")
                 (bus/new-reply ::bus/no-response :QUIT))

       (f bot-session wrk req handler-args)))))

(defn generic-data-handler
  "Handles additional data which are a result of a request. It's like side effect for
  the request, regardless of its reply. When session object is returned then
  bot-specific data handlers should not interfere (processing is done) and just
  replace session with the returned one. When nil is returned then bot-specific data
  handlers should exit the loop and finish execution. All other data is passed as
  is."
  [data bot-session]
  (case data

    :PAUSE (if (= :PAUSED (:previous-stage bot-session))
             bot-session
             (assoc bot-session
                    :previous-stage (:stage bot-session)
                    :stage :PAUSED))

    :RUN (if-not (= :PAUSED (:stage bot-session))
           bot-session
           (assoc bot-session :stage (:previous-stage bot-session)))

    nil bot-session

    data))

(defn handle-request
  "Handles requests. Returns an Outcome object. Passes control to a chosen request
  handler f to prepare the response. Updates the returned map with data-handler if
  data is present in the outcome."
  [bot-session f data-handler wrk req & handler-args]
  (if (bus/request? req)
    (let [gc (partial generic-control bot-session f)
          oc (apply bus/handle-request wrk req gc handler-args)]
      (if (some? (.data ^Outcome oc))
        (update oc :data data-handler bot-session)
        oc))
    (do (log/warn (str "Bot is ignoring unknown control message: " req))
        (Thread/sleep 500)
        bus/empty-outcome)))

;;
;; remote commands
;;

(defn stop!
  [wrk]
  (let [wrk (bus/worker wrk)]
    (when (bus/stop-worker wrk (bus/new-request wrk ::stop)) true)))

(defn command
  [wrk command & args]
  (if (valuable? command)
    (:body (apply bus/request->response wrk (ensure-keyword command) args))))

(defn get-data
  [wrk k & args]
  (if (valuable? k)
    (:body (apply bus/request->response wrk ::data (ensure-keyword k) args))))

(defn get-data!
  [wrk k & args]
  (if (valuable? k)
    (:body (apply bus/request->response wrk ::data! (ensure-keyword k) args))))

(defn update-local-config!
  [wid]
  (let [wid (if (bus/worker? wid) (:id wid) wid)]
    (if wid
      (let [res (bus/request->response wid ::config)]
        (when (bus/response? res)
          (log/debug (str "Updating supervised config of " (symbol wid)))
          (bus/update-config! wid (.body ^Response res)))))))

(defn get-config  [wrk] (command wrk ::config))
(defn get-session [wrk] (command wrk ::session))
(defn pause       [wrk] (command wrk ::pause))
(defn ping        [wrk] (command wrk ::ping))
