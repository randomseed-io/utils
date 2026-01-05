(ns io.randomseed.utils.log.logback

  ^{:doc    "Random utils, logback support functions."
    :author "Paweł Wilk"
    :added  "1.0.0"}

  (:import (org.slf4j LoggerFactory)
           (ch.qos.logback.classic Logger LoggerContext Level)
           (ch.qos.logback.core Appender)))

(defn- appender-seq
  "Returns a seq of appenders attached to given logger."
  [^ch.qos.logback.classic.Logger logger]
  (iterator-seq (.iteratorForAppenders logger)))

(defn ^LoggerContext logger-context
  []
  (let [f (LoggerFactory/getILoggerFactory)]
    (when-not (instance? LoggerContext f)
      (throw (ex-info "Not a Logback LoggerContext" {:factory (class f)})))
    ^LoggerContext f))

(defn detach-appenders-by-prefix!
  [^String prefix]
  (let [^LoggerContext ctx (logger-context)
        ^Logger root (.getLogger ctx Logger/ROOT_LOGGER_NAME)]
    (doseq [^Appender app (iterator-seq (.iteratorForAppenders root))
            :let [nm (.getName app)]
            :when (and nm (.startsWith ^String nm prefix))]
      (.detachAppender root app)
      (try (.stop app) (catch Throwable _))))
  :ok)

(defn snapshot-logback!
  "Captures current logback runtime state (safe, no reset):
   - root logger level
   - root appenders (objects)
   - logger levels map (explicit levels only)
   - additive flags (optional)
  Returns a snapshot map."
  []
  (let [^ch.qos.logback.classic.LoggerContext ctx (logger-context)
        ^ch.qos.logback.classic.Logger root (.getLogger ctx ch.qos.logback.classic.Logger/ROOT_LOGGER_NAME)
        root-apps (vec (appender-seq root))
        loggers (vec (.getLoggerList ctx))
        lvl-map (into {}
                      (keep (fn [^ch.qos.logback.classic.Logger l]
                              (when-let [lv (.getLevel l)]
                                [(.getName l) (.toString lv)])))
                      loggers)
        add-map (into {}
                      (map (fn [^ch.qos.logback.classic.Logger l]
                             [(.getName l) (.isAdditive l)]))
                      loggers)]
    {:root-level     (some-> (.getLevel root) .toString)
     :root-appenders root-apps
     :levels         lvl-map
     :additive       add-map}))

(defn restore-logback!
  [{:keys [root-level root-appenders levels additive]}]
  (let [^ch.qos.logback.classic.LoggerContext ctx (logger-context)
        ^ch.qos.logback.classic.Logger root (.getLogger ctx ch.qos.logback.classic.Logger/ROOT_LOGGER_NAME)]
    (doseq [app (appender-seq root)]
      (.detachAppender root ^ch.qos.logback.core.Appender app))
    (doseq [app root-appenders]
      (.addAppender root ^ch.qos.logback.core.Appender app))
    (.setLevel root (when root-level (ch.qos.logback.classic.Level/toLevel ^String root-level)))

    (doseq [[nm lv] levels]
      (let [l (.getLogger ctx ^String nm)]
        (.setLevel ^ch.qos.logback.classic.Logger l (ch.qos.logback.classic.Level/toLevel ^String lv))))

    (doseq [[nm add?] additive]
      (let [l (.getLogger ctx ^String nm)]
        (.setAdditive ^ch.qos.logback.classic.Logger l (boolean add?))))
    :ok))
