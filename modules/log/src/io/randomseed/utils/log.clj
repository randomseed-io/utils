(ns

    ^{:doc    "Random utils, opinionated logging support functions."
      :author "Paweł Wilk"
      :added  "1.0.0"}

    io.randomseed.utils.log

  (:require [io.randomseed.utils             :as     util]
            [io.randomseed.utils.fs          :as       fs]
            [io.randomseed.utils.map         :as      map]
            [io.randomseed.utils.log.logback :as  logback]
            [cheshire.core                   :as     json]
            [cheshire.parse                  :as       cp]
            [unilog.config                   :as   unilog]
            [cambium.codec                   :as    codec]
            [cambium.core                    :as      log]
            [cambium.mdc                     :as     mlog]
            [logback-bundle.json.flat-layout :as     flat])

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
  "Configures the JSON decoder for structured logging: enables big-decimal parsing,
  installs the cambium destringify codec and sets the global `FlatJsonLayout` decoder."
  []
  (alter-var-root #'cp/*use-bigdecimals?* (constantly true))
  (flat/set-decoder! codec/destringify-val)
  (FlatJsonLayout/setGlobalDecoder
   (reify ValueDecoder
     (decode [_ encoded-value]
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

(defmethod unilog/build-appender :console-error
  [config]
  (assoc config :appender (doto (ConsoleAppender.)
                            (.setName "io.randomseed.utils.log.console-error")
                            (.setWithJansi false)
                            (.setTarget "System.err")
                            (.addFilter (doto (ThresholdFilter.)
                                          (.setLevel "ERROR")
                                          (.start))))))

(defmethod unilog/build-appender :console-warn
  [config]
  (assoc config :appender (doto (ConsoleAppender.)
                            (.setName "io.randomseed.utils.log.console-warn")
                            (.setWithJansi false)
                            (.setTarget "System.err")
                            (.addFilter (doto (ThresholdFilter.)
                                          (.setLevel "WARN")
                                          (.start))))))

(defmethod unilog/build-appender :console-info
  [config]
  (assoc config :appender (doto (ConsoleAppender.)
                            (.setName "io.randomseed.utils.log.console-error")
                            (.setWithJansi false)
                            (.setTarget "System.err")
                            (.addFilter (doto (ThresholdFilter.)
                                          (.setLevel "INFO")
                                          (.start))))))

(defmethod unilog/build-appender :console-debug
  [config]
  (assoc config :appender (doto (ConsoleAppender.)
                            (.setName "io.randomseed.utils.log.console-debug")
                            (.setWithJansi false)
                            (.setTarget "System.err")
                            (.addFilter (doto (ThresholdFilter.)
                                          (.setLevel "DEBUG")
                                          (.start))))))

(defmethod unilog/build-appender :console-trace
  [config]
  (assoc config :appender (doto (ConsoleAppender.)
                            (.setName "io.randomseed.utils.log.console-trace")
                            (.setWithJansi false)
                            (.setTarget "System.err")
                            (.addFilter (doto (ThresholdFilter.)
                                          (.setLevel "TRACE")
                                          (.start))))))

;;
;; Logging wrappers
;;

(defmacro log-context
  "Evaluates `body` with additional MDC logging `context` (a map)."
  [context & body]
  (apply #'log/with-logging-context &form &env context body))

(defmacro with-ctx
  "Alias for `log-context`. Evaluates `body` with additional MDC logging `context`."
  [context & body]
  (apply #'log/with-logging-context &form &env context body))

(defmacro log
  "Logs a message at the given `level`. Delegates to `cambium.core/log`."
  ([level msg-or-throwable]        (#'log/log &form &env level msg-or-throwable))
  ([level mdc throwable msg]       (#'log/log &form &env level mdc throwable msg))
  ([logger level msg-or-throwable] (#'log/log &form &env logger level msg-or-throwable))
  ([logger level mdc-or-throwable throwable msg]
   (#'log/log &form &env logger level mdc-or-throwable throwable msg)))

(defmacro trace
  "Logs a message at TRACE level."
  ([msg-or-throwable]     (#'log/trace &form &env msg-or-throwable))
  ([mdc-or-throwable msg] (#'log/trace &form &env mdc-or-throwable msg))
  ([mdc throwable msg]    (#'log/trace &form &env mdc throwable msg)))

(defmacro debug
  "Logs a message at DEBUG level."
  ([msg-or-throwable]     (#'log/debug &form &env msg-or-throwable))
  ([mdc-or-throwable msg] (#'log/debug &form &env mdc-or-throwable msg))
  ([mdc throwable msg]    (#'log/debug &form &env mdc throwable msg)))

(defmacro info
  "Logs a message at INFO level."
  ([msg-or-throwable]     (#'log/info &form &env msg-or-throwable))
  ([mdc-or-throwable msg] (#'log/info &form &env mdc-or-throwable msg))
  ([mdc throwable msg]    (#'log/info &form &env mdc throwable msg)))

(defmacro warn
  "Logs a message at WARN level."
  ([msg-or-throwable]     (#'log/warn &form &env msg-or-throwable))
  ([mdc-or-throwable msg] (#'log/warn &form &env mdc-or-throwable msg))
  ([mdc throwable msg]    (#'log/warn &form &env mdc throwable msg)))

(defmacro warning
  "Alias for `warn`. Logs a message at WARN level."
  ([msg-or-throwable]     (#'log/warn &form &env msg-or-throwable))
  ([mdc-or-throwable msg] (#'log/warn &form &env mdc-or-throwable msg))
  ([mdc throwable msg]    (#'log/warn &form &env mdc throwable msg)))

(defmacro error
  "Logs a message at ERROR level."
  ([msg-or-throwable]     (#'log/error &form &env msg-or-throwable))
  ([mdc-or-throwable msg] (#'log/error &form &env mdc-or-throwable msg))
  ([mdc throwable msg]    (#'log/error &form &env mdc throwable msg)))

(defmacro fatal
  "Logs a message at FATAL level."
  ([msg-or-throwable]     (#'log/fatal &form &env msg-or-throwable))
  ([mdc-or-throwable msg] (#'log/fatal &form &env mdc-or-throwable msg))
  ([mdc throwable msg]    (#'log/fatal &form &env mdc throwable msg)))

(defmacro msg-with-val
  "Logs `msg` (and optional extra parts) at INFO level. When extra arguments are
  present, the last one is returned as the expression value."
  [msg & more]
  (if more
    (list 'do (#'log/info &form &env (list* #'util/some-str-spc msg (drop-last more))) (last more))
    (#'log/info &form &env (list #'clojure.core/str (list #'util/some-str msg)))))

(defmacro err-with-val
  "Logs `msg` (and optional extra parts) at ERROR level. When extra arguments are
  present, the last one is returned as the expression value."
  [msg & more]
  (if more
    (list 'do (#'log/error &form &env (list* #'util/some-str-spc msg (drop-last more))) (last more))
    (#'log/error &form &env (list #'clojure.core/str (list #'util/some-str msg)))))

(defmacro msg
  "Logs a space-joined message at INFO level using `some-str-spc`."
  [msg & more]
  (if more
    (#'log/info &form &env (list* #'util/some-str-spc msg more))
    (#'log/info &form &env (list  #'clojure.core/str (list #'util/some-str msg)))))

(defmacro err
  "Logs a space-joined message at ERROR level using `some-str-spc`."
  [msg & more]
  (if more
    (#'log/error &form &env (list* #'util/some-str-spc msg more))
    (#'log/error &form &env (list  #'clojure.core/str (list #'util/some-str msg)))))

(defmacro wrn
  "Logs a space-joined message at WARN level using `some-str-spc`."
  [msg & more]
  (if more
    (#'log/warn &form &env (list* #'util/some-str-spc msg more))
    (#'log/warn &form &env (list  #'clojure.core/str (list #'util/some-str msg)))))

(defmacro dbg
  "Logs a space-joined message at DEBUG level using `some-str-spc`."
  [msg & more]
  (if more
    (#'log/debug &form &env (list* #'util/some-str-spc msg more))
    (#'log/debug &form &env (list  #'clojure.core/str (list #'util/some-str msg)))))

;;
;; Uncaught exception handler
;;

(def ^{:dynamic true :doc "When `true`, indicates the current exception has already been logged and should not be logged again by outer handlers."}
  *already-logged* false)

;; (defn- set-exception-handler!
;;   []
;;   (Thread/setDefaultUncaughtExceptionHandler
;;    (reify Thread$UncaughtExceptionHandler
;;      (uncaughtException [_ thread ex]
;;        (if-not *already-logged*
;;          (log/error {:thread (.getName thread)} (pr-str ex)))
;;        (prn ex)))))
;; (set-exception-handler!)

;;
;; Exceptions catching loop
;;

(defmacro log-exceptions
  "Wraps `body` in a try/catch that logs any `Throwable` at ERROR level (with the
  exception stringified in the MDC context) and re-throws it with `*already-logged*`
  bound to `true` so outer handlers can avoid duplicate logging."
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
  "Installs a cambium context transformer derived from `transform-map`.
  Returns the previous transformer (or nil if nothing changed)."
  [transform-map]
  (when (seq transform-map)
    (let [prev cambium.core/transform-context
          xf   (fn [context]
                 (map/update-values-or-seqs-recur context transform-map))]
      (alter-var-root #'cambium.core/transform-context (constantly xf))
      prev)))

;;
;; Logging start
;;

(defn start!
  "Starts the logging subsystem by delegating to `unilog/start-logging!` with the
  given `config` map."
  [config]
  (unilog/start-logging! config))

;;
;; System handlers
;;

(def ^{:doc "Default logging configuration map: INFO level with console output enabled."}
  default-config {:level "info" :console true})

(defn preprocess-config
  "Pre-processes logging `config` by expanding Java property placeholders in `:file`
  paths of each appender entry. Marks the config as `:preprocessed` to avoid
  repeated processing. Returns `config` unchanged when no `:appenders` key is present
  or the config has already been preprocessed."
  [config]
  (if-some [ap (:appenders config)]
    (if (:preprocessed config)
      config
      (assoc config
             :preprocessed true
             :appenders (mapv #(if (:file %) (update % :file fs/parse-java-properties) %) ap)))
    config))

(defn init!
  "Initialises the logging subsystem. Snapshots the current Logback state, detaches
  previously installed appenders, configures the JSON decoder and the context
  transformer, then starts unilog. Returns a map with `:config`,
  `:previous-ctx-transformer`, `:previous-logback`, and `:unilog` keys suitable for
  passing to `stop!` to restore the prior state."
  [config]
  (let [config          (if (and (contains? config :config) (contains? config :unilog)) (get config :config) config)
        config          (preprocess-config config)
        ;; pname        (:profile (:system config))
        ctx-transformer (map/map-of-vectors-invert-flatten (:context-transformer config))
        prev-logback    (logback/snapshot-logback!)
        prev-ctx        cambium.core/transform-context]
    (logback/detach-appenders-by-prefix! "io.randomseed.utils.log.")
    (initialize-json-decoder!)
    (initialize-context-transformer! ctx-transformer)
    (let [unilog-ret (start! config)]
      {:config                   config
       :previous-ctx-transformer prev-ctx
       :previous-logback         prev-logback
       :unilog                   unilog-ret})))

(defn stop!
  "Tears down the logging subsystem and restores the state captured by `init!`.
  Detaches appenders prefixed with `\"io.randomseed.utils.log.\"`, restores the
  previous context transformer and the previous Logback snapshot. Returns `nil`."
  [{:keys [previous-ctx-transformer previous-logback]}]
  (logback/detach-appenders-by-prefix! "io.randomseed.utils.log.")
  (when previous-ctx-transformer
    (alter-var-root #'cambium.core/transform-context (constantly previous-ctx-transformer)))
  (when previous-logback
    (logback/restore-logback! previous-logback))
  nil)
