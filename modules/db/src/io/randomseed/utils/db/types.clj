(ns

    ^{:doc    "Opinionated database type conversions."
      :author "Paweł Wilk"
      :added  "2.0.9"}

    io.randomseed.utils.db.types

  (:require [io.randomseed.utils.db]
            [clojure.edn            :as   edn]
            [clojure.java.classpath :as    cp]
            [clojure.java.io        :as    io]
            [potemkin               :as     p]
            [next.jdbc.result-set   :as    rs]
            [next.jdbc.prepare      :as    jp]
            [clj-uuid               :as  uuid])

  (:import  (java.sql  Blob PreparedStatement Timestamp)
            (java.util UUID Calendar TimeZone)))

(set! *warn-on-reflection* true)

(defonce ^{:tag TimeZone
            :doc "Cached UTC `TimeZone` instance used for timestamp conversions."}
  utc-time-zone
  (TimeZone/getTimeZone "UTC"))

;; Extensible adapter registry (for optional modules)

(def ^{:doc "Classpath resource path used by optional DB adapter registration."}
  adders-resource-path
  "io/randomseed/utils/db/types/adders.edn")

(defonce ^{:private true
           :doc     "Registry of external reader adders. Each entry should be a var or a no-arg fn."}
  registered-reader-adders
  (atom []))

(defonce ^{:private true
           :doc     "Registry of external setter adders. Each entry should be a var or a no-arg fn."}
  registered-setter-adders
  (atom []))

(defonce ^{:private true
           :doc     "Guard ensuring optional adders are autoloaded at most once per runtime."}
  optional-adders-autoloaded?
  (atom false))

(defn- register-adder!
  [registry adder]
  (when adder
    (swap! registry
           (fn [v]
             (if (some #(identical? % adder) v)
               v
               (conj v adder)))))
  nil)

(defn register-reader-adder!
  "Registers a no-arg reader-adder function (or var). Registered adders are executed
  by `add-all-readers`."
  [adder]
  (register-adder! registered-reader-adders adder))

(defn register-setter-adder!
  "Registers a no-arg setter-adder function (or var). Registered adders are executed
  by `add-all-setters`."
  [adder]
  (register-adder! registered-setter-adders adder))

(defn- resolve-adder
  [sym]
  (try
    (requiring-resolve sym)
    (catch Throwable _
      nil)))

(defn- normalize-adders
  [v]
  (let [m (if (map? v) v {})]
    {:readers (->> (:readers m) (filter symbol?) vec)
     :setters (->> (:setters m) (filter symbol?) vec)}))

(defn- parse-adders-resource
  [r]
  (try
    (normalize-adders (edn/read-string (slurp r)))
    (catch Throwable _
      {:readers [] :setters []})))

(defn- classpath-resources
  []
  (let [dirs    (for [dir (cp/classpath-directories)
                      :let [f (io/file dir adders-resource-path)]
                      :when (.isFile f)]
                  f)
        jars    (for [^java.util.jar.JarFile jar (cp/classpath-jarfiles)
                      :when (some #(= adders-resource-path %) (cp/filenames-in-jar jar))]
                  (str "jar:" (-> (.getName jar) io/file .toURI str) "!/" adders-resource-path))
        loader  (try
                  (let [^ClassLoader cl (or (.getContextClassLoader (Thread/currentThread))
                                            (clojure.lang.RT/baseLoader))]
                    (enumeration-seq (.getResources cl adders-resource-path)))
                  (catch Throwable _
                    []))]
    (distinct (concat dirs jars loader))))

(defn- discover-adder-symbols
  []
  (let [acc (reduce
             (fn [{:keys [readers setters]} url]
               (let [{rs :readers ss :setters} (parse-adders-resource url)]
                 {:readers (into readers rs)
                  :setters (into setters ss)}))
             {:readers [] :setters []}
             (classpath-resources))]
    {:readers (vec (distinct (:readers acc)))
     :setters (vec (distinct (:setters acc)))}))

(defn- autoload-optional-adders!
  []
  (when (compare-and-set! optional-adders-autoloaded? false true)
    (let [{:keys [readers setters]} (discover-adder-symbols)]
      (doseq [sym readers]
        (when-let [adder (resolve-adder sym)]
          (register-reader-adder! adder)))
      (doseq [sym setters]
        (when-let [adder (resolve-adder sym)]
          (register-setter-adder! adder))))))

(defn- invoke-adders!
  [adders]
  (doseq [adder adders]
    (let [f (cond
              (var? adder) @adder
              (fn? adder)  adder
              :else nil)]
      (when (fn? f) (f)))))

;; Type conversions when reading from a DB

(defonce
  ^{:arglists '([])
    :doc      "Extends `next.jdbc.result-set/ReadableColumn` protocol to support date/time
  conversions so `java.sql.Date` data are passed as-is and `java.sql.Timestamp`
  data are converted to `java.sql.Timestamp`."}
  add-reader-date
  (fn []
    (extend-protocol rs/ReadableColumn

      java.sql.Date

      (read-column-by-label [^java.sql.Date v _]     ^java.sql.Date v)
      (read-column-by-index [^java.sql.Date v _2 _3] ^java.sql.Date v)

      java.sql.Timestamp

      (read-column-by-label [^Timestamp v _]     (.toInstant ^Timestamp v))
      (read-column-by-index [^Timestamp v _2 _3] (.toInstant ^Timestamp v)))))

(defonce
  ^{:arglists '([])
    :doc      "Extends `next.jdbc.result-set/ReadableColumn` protocol to support binary large
  object (BLOB) conversions, so `java.sql.Blob` data are converted to an array of
  bytes (`bytes[]`)."}
  add-reader-blob
  (fn []
    (extend-protocol rs/ReadableColumn

      java.sql.Blob

      (read-column-by-label [^Blob v _]     (.getBytes ^Blob v (long 1) ^long (.length ^Blob v)))
      (read-column-by-index [^Blob v _2 _3] (.getBytes ^Blob v (long 1) ^long (.length ^Blob v))))))

(defn add-all-readers
  "Adds all opinionated readers by calling `add-reader-date`, `add-reader-blob`, and
  reader adders registered with `register-reader-adder!`."
  []
  (autoload-optional-adders!)
  (add-reader-date)
  (add-reader-blob)
  (invoke-adders! @registered-reader-adders))

;; Type conversions when writing to a DB

(defonce
  ^{:arglists '([])
    :doc      "Extends `next.jdbc.prepare/SettableParameter` protocol to support date/time
  conversions so `java.sql.Date` data are saved using `.setDate` method and
  `java.sql.Timestamp`, `java.time.Instant`, `java.time.ZonedTime`,
  `java.time.LocalDate`, `java.time.LocalDateTime` and `java.util.Date` are saved with
  `.setTimestamp`. Also note that `java.time.Instant`, `java.time.ZonedDateTime` and
  `java.util.Date` are explicitly converted to UTC before saving."}
  add-setter-date
  (fn []
    (extend-protocol jp/SettableParameter

      java.sql.Date

      (set-parameter [^java.sql.Date v ^PreparedStatement ps ^long i]
        (.setDate ^PreparedStatement ps i ^java.sql.Date v))

      java.sql.Timestamp

      (set-parameter [^Timestamp v ^PreparedStatement ps ^long i]
        (.setTimestamp ^PreparedStatement ps i ^java.sql.Timestamp v))

      java.time.Instant

      (set-parameter [^java.time.Instant v ^PreparedStatement ps ^long i]
        (.setTimestamp ^PreparedStatement ps i
                       ^Timestamp (Timestamp/from ^java.time.Instant v)
                       ^Calendar  (Calendar/getInstance ^TimeZone utc-time-zone)))

      java.time.ZonedDateTime

      (set-parameter [^java.time.ZonedDateTime v ^PreparedStatement ps ^long i]
        (.setTimestamp ^PreparedStatement ps i
                       ^Timestamp (Timestamp/from ^java.time.Instant (.toInstant ^java.time.ZonedDateTime v))
                       ^Calendar  (Calendar/getInstance ^TimeZone utc-time-zone)))

      java.time.LocalDate

      (set-parameter [^java.time.LocalDate v ^PreparedStatement ps ^long i]
        (.setTimestamp ^PreparedStatement ps i
                       ^Timestamp (Timestamp/valueOf ^java.time.LocalDateTime (.atStartOfDay ^java.time.LocalDate v))
                       ^Calendar  (Calendar/getInstance)))

      java.time.LocalDateTime

      (set-parameter [^java.time.LocalDateTime v ^PreparedStatement ps ^long i]
        (.setTimestamp ^PreparedStatement ps i
                       ^Timestamp (Timestamp/valueOf ^java.time.LocalDateTime v)
                       ^Calendar  (Calendar/getInstance)))

      java.util.Date

      (set-parameter [^java.util.Date v ^PreparedStatement ps ^long i]
        (.setTimestamp ^PreparedStatement ps i
                       ^Timestamp (Timestamp/from ^java.time.Instant (.toInstant ^java.util.Date v))
                       ^Calendar  (Calendar/getInstance ^TimeZone utc-time-zone))))))

(defonce
  ^{:arglists '([])
    :doc      "Extends `next.jdbc.prepare/SettableParameter` protocol to support UUID
  conversions so `java.util.UUID` data are converted to an array of bytes and then
  saved using `.setBytes` method."}
  add-setter-uuid
  (fn []
    (extend-protocol jp/SettableParameter

      java.util.UUID

      (set-parameter [^UUID v ^PreparedStatement ps ^long i]
        (.setBytes ^PreparedStatement ps i ^bytes (uuid/to-byte-array ^UUID v))))))

(defn add-all-setters
  "Adds all opinionated setters by calling core setter adders and setter adders
  registered with `register-setter-adder!`."
  []
  (autoload-optional-adders!)
  (add-setter-date)
  (add-setter-uuid)
  (invoke-adders! @registered-setter-adders))

(defn add-all-accessors
  "Adds all opinionated readers and setters by calling `add-all-readers` and `add-all-setters`."
  []
  (add-all-readers)
  (add-all-setters))

;; Exposed aliases for builder and conversion functions

(p/import-vars [io.randomseed.utils.db
                to-lisp-simple to-snake-simple to-lisp to-snake
                to-lisp-slashed to-snake-slashed
                opts-simple-map opts-map opts-simple-vec opts-vec
                opts-slashed-map opts-slashed-vec])
