(ns

    ^{:doc    "Random utils, opinionated logging support functions."
      :author "Paweł Wilk"
      :added  "1.0.0"}

    io.randomseed.utils.log

  (:require [clojure.string                  :as      str]
            [trptr.java-wrapper.locale       :as        l]
            [tick.core                       :as        t]
            [io.randomseed.utils             :as     util]
            [io.randomseed.utils.fs          :as       fs]
            [io.randomseed.utils.map         :as      map]
            [cheshire.core                   :as     json]
            [cheshire.parse                  :as       cp]
            [unilog.config                   :as   unilog]
            [cambium.codec                   :as    codec]
            [cambium.core                    :as      log]
            [cambium.mdc                     :as     mlog]
            [logback-bundle.json.flat-layout :as     flat]
            [buddy.core.hash                 :as     hash]
            [buddy.core.codecs               :as   codecs])

  (:import  [logback_bundle.json                 FlatJsonLayout ValueDecoder]
            [ch.qos.logback.contrib.jackson      JacksonJsonFormatter]
            [ch.qos.logback.core.encoder         LayoutWrappingEncoder]
            [ch.qos.logback.contrib.json.classic JsonLayout]
            [ch.qos.logback.classic.filter       ThresholdFilter]
            [ch.qos.logback.classic.encoder      PatternLayoutEncoder]
            [ch.qos.logback.core                 ConsoleAppender]
            [java.nio.charset                    Charset]))

;;
;; Flat JSON decoder configuration
;;

(defn initialize-json-decoder!
  []
  (alter-var-root #'cp/*use-bigdecimals?* (constantly true))
  (flat/set-decoder! codec/destringify-val)
  (FlatJsonLayout/setGlobalDecoder
   (reify ValueDecoder
     (decode [this encoded-value]
       (json/parse-string encoded-value)))))

;;
;; Custom encoders
;;

(defmethod unilog/build-encoder :pattern-cool
  [{:keys [pattern] :as config}]
  (let [encoder (doto (PatternLayoutEncoder.)
                  (.setCharset (Charset/forName "UTF-8"))
                  (.setPattern (or pattern unilog/default-pattern)))]
    (assoc config :encoder encoder)))

(defmethod unilog/build-encoder :json-console
  [config]
  (assoc config :encoder (doto (LayoutWrappingEncoder.)
                           (.setCharset (Charset/forName "UTF-8"))
                           (.setLayout  (doto (FlatJsonLayout.)
                                          (.setIncludeMDC                true)
                                          (.setIncludeException          true)
                                          (.setAppendLineSeparator       true)
                                          (.setTimestampFormatTimezoneId "Etc/UTC")
                                          (.setTimestampFormat           "%date{yyyy-MM-dd HH:mm:ss','SSS'Z', UTC}")
                                          (.setJsonFormatter (doto (JacksonJsonFormatter.)
                                                               (.setPrettyPrint true))))))))

(defmethod unilog/build-encoder :json-log
  [config]
  (assoc config :encoder (doto (LayoutWrappingEncoder.)
                           (.setCharset (Charset/forName "UTF-8"))
                           (.setLayout  (doto (FlatJsonLayout.)
                                          (.setIncludeMDC                 true)
                                          (.setIncludeException           true)
                                          (.setAppendLineSeparator        true)
                                          (.setTimestampFormatTimezoneId "Etc/UTC")
                                          (.setJsonFormatter (doto (JacksonJsonFormatter.)
                                                               (.setPrettyPrint false))))))))

;;
;; Custom appenders
;;

(defmethod unilog/build-appender :console-info
  [config]
  (assoc config :appender (doto (ConsoleAppender.)
                            (.setWithJansi true)
                            (.addFilter (doto (ThresholdFilter.)
                                          (.setLevel "INFO")
                                          (.start))))))

;;
;; Logging wrappers
;;

(defmacro log-context
  [context & body]
  (apply #'log/with-logging-context &form &env context body))

(defmacro with-ctx
  [context & body]
  (apply #'log/with-logging-context &form &env context body))

(defmacro log
  ([level msg-or-throwable]        (#'log/log &form &env level msg-or-throwable))
  ([level mdc throwable msg]       (#'log/log &form &env level mdc throwable msg))
  ([logger level msg-or-throwable] (#'log/log &form &env logger level msg-or-throwable))
  ([logger level mdc-or-throwable throwable msg]
   (#'log/log &form &env logger level mdc-or-throwable throwable msg)))

(defmacro trace
  ([msg-or-throwable]     (#'log/trace &form &env msg-or-throwable))
  ([mdc-or-throwable msg] (#'log/trace &form &env mdc-or-throwable msg))
  ([mdc throwable msg]    (#'log/trace &form &env mdc throwable msg)))

(defmacro debug
  ([msg-or-throwable]     (#'log/debug &form &env msg-or-throwable))
  ([mdc-or-throwable msg] (#'log/debug &form &env mdc-or-throwable msg))
  ([mdc throwable msg]    (#'log/debug &form &env mdc throwable msg)))

(defmacro info
  ([msg-or-throwable]     (#'log/info &form &env msg-or-throwable))
  ([mdc-or-throwable msg] (#'log/info &form &env mdc-or-throwable msg))
  ([mdc throwable msg]    (#'log/info &form &env mdc throwable msg)))

(defmacro warn
  ([msg-or-throwable]     (#'log/warn &form &env msg-or-throwable))
  ([mdc-or-throwable msg] (#'log/warn &form &env mdc-or-throwable msg))
  ([mdc throwable msg]    (#'log/warn &form &env mdc throwable msg)))

(defmacro warning
  ([msg-or-throwable]     (#'log/warn &form &env msg-or-throwable))
  ([mdc-or-throwable msg] (#'log/warn &form &env mdc-or-throwable msg))
  ([mdc throwable msg]    (#'log/warn &form &env mdc throwable msg)))

(defmacro error
  ([msg-or-throwable]     (#'log/error &form &env msg-or-throwable))
  ([mdc-or-throwable msg] (#'log/error &form &env mdc-or-throwable msg))
  ([mdc throwable msg]    (#'log/error &form &env mdc throwable msg)))

(defmacro fatal
  ([msg-or-throwable]     (#'log/fatal &form &env msg-or-throwable))
  ([mdc-or-throwable msg] (#'log/fatal &form &env mdc-or-throwable msg))
  ([mdc throwable msg]    (#'log/fatal &form &env mdc throwable msg)))

(defmacro msg-with-val
  [msg & more]
  (if more
    (list 'do (#'log/info &form &env (list* #'util/some-str-spc msg (drop-last more))) (last more))
    (#'log/info &form &env (list #'clojure.core/str (list #'util/some-str msg)))))

(defmacro err-with-val
  [msg & more]
  (if more
    (list 'do (#'log/error &form &env (list* #'util/some-str-spc msg (drop-last more))) (last more))
    (#'log/error &form &env (list #'clojure.core/str (list #'util/some-str msg)))))

(defmacro msg
  [msg & more]
  (if more
    (#'log/info &form &env (list* #'util/some-str-spc msg more))
    (#'log/info &form &env (list  #'clojure.core/str (list #'util/some-str msg)))))

(defmacro err
  [msg & more]
  (if more
    (#'log/error &form &env (list* #'util/some-str-spc msg more))
    (#'log/error &form &env (list  #'clojure.core/str (list #'util/some-str msg)))))

(defmacro wrn
  [msg & more]
  (if more
    (#'log/warn &form &env (list* #'util/some-str-spc msg more))
    (#'log/warn &form &env (list  #'clojure.core/str (list #'util/some-str msg)))))

(defmacro dbg
  [msg & more]
  (if more
    (#'log/debug &form &env (list* #'util/some-str-spc msg more))
    (#'log/debug &form &env (list  #'clojure.core/str (list #'util/some-str msg)))))

;;
;; Uncaught exception handler
;;

(def ^:dynamic *already-logged* false)

;; (defn- set-exception-handler!
;;   []
;;   (Thread/setDefaultUncaughtExceptionHandler
;;    (reify Thread$UncaughtExceptionHandler
;;      (uncaughtException [_ thread ex]
;;        (when-not *already-logged*
;;          (log/error {:thread (.getName thread)} (pr-str ex)))
;;        (prn ex)))))
;; (set-exception-handler!)

;;
;; Exceptions catching loop
;;

(defmacro log-exceptions
  [& body]
  `(try
     ~@body
     (catch Throwable ex#
       (cambium.core/error {:exception (pr-str ex#)} (.getMessage ex#))
       (binding [*already-logged* true]
         (throw ex#)))))

;;
;; Context processing
;;

(defn initialize-context-transformer!
  [transform-map]
  (when (seq transform-map)
    (alter-var-root
     #'cambium.core/transform-context
     (fn [_]
       (fn [context]
         (map/update-values-or-seqs-recur context transform-map))))))

;;
;; Logging start
;;

(defn start!
  [config]
  (unilog/start-logging! config))

;;
;; System handlers
;;

(def default-config {:level "info" :console true})

(defn preprocess-config
  [config]
  (if-some [ap (:appenders config)]
    (if (:preprocessed config)
      config
      (assoc config
             :preprocessed true
             :appenders (mapv #(if (:file %) (update % :file fs/parse-java-properties) %) ap)))
    config))

(defn init!
  [config]
  (let [config          (preprocess-config config)
        pname           (:profile (:system config))
        ctx-transformer (map/map-of-vectors-invert-flatten (:context-transformer config))]
    (initialize-json-decoder!)
    (initialize-context-transformer! ctx-transformer)
    (start! config)))

(defn stop!
  [config]
  (unilog/start-logging! default-config)
  default-config)
