(ns

    ^{:doc    "Random utilities, date and time functions."
      :author "Paweł Wilk"
      :added  "1.0.0"}

    io.randomseed.utils.time

  (:refer-clojure :exclude [parse-long uuid random-uuid])

  (:import  [java.time Instant Duration ZoneRegion]
            [java.util Date Calendar])

  (:require [tick.core           :as       t]
            [tick.protocols      :as      tp]
            [clojure.string      :as     str]
            [io.randomseed.utils :as       u]))

(def ^:const duration-map
  {:ns           :nanos
   "ns"          :nanos
   :us           :micros
   "us"          :micros
   :ms           :millis
   "ms"          :millis
   :yoctosecond  :yoctoseconds
   :zeptosecond  :zeptoseconds
   :attosecond   :attoseconds
   :femtosecond  :femtoseconds
   :picosecond   :picos
   :picoseconds  :picos
   :pico         :picos
   :nanosecond   :nanos
   :nanoseconds  :nanos
   :nano         :nanos
   :microsecond  :micros
   :microseconds :micros
   :micro        :micros
   :millisecond  :millis
   :milliseconds :millis
   :milli        :millis
   :centisecond  :centiseconds
   :decisecond   :deciseconds
   :second       :seconds
   :sec          :seconds
   :secs         :seconds
   :minute       :minutes
   :min          :minutes
   :mins         :minutes
   :quadrans     :quadranses
   :hour         :hours
   :day          :days
   :night        :nights
   :week         :weeks
   :weekend      :weekends
   :month        :months
   :quarter      :quarters
   :year         :years
   :century      :centuries
   :millenium    :millenia
   :era          :eras
   :eon          :eons})

(def ^:const unit-to-efn
  {:nanos   t/nanos
   :micros  t/micros
   :millis  t/millis
   :seconds t/seconds
   :minutes t/minutes
   :hours   t/hours
   :days    t/days
   :months  t/months
   :years   t/years})

(def ^:private parse-duration-unit-alias
  "Alias-map used *only* by parse-duration.
  Intentionally does NOT include single-letter keyword shorthands like :m/:s,
  so that [3 :m] continues to throw (matching tick.core/new-duration behavior)."
  {;; nanos
   :ns           :nanos
   :nano         :nanos
   :nanos        :nanos
   :nanosecond   :nanos
   :nanoseconds  :nanos
   ;; micros
   :us           :micros
   :micro        :micros
   :micros       :micros
   :microsecond  :micros
   :microseconds :micros
   ;; millis
   :ms           :millis
   :milli        :millis
   :millis       :millis
   :millisecond  :millis
   :milliseconds :millis
   ;; seconds
   :s            :seconds
   :sec          :seconds
   :secs         :seconds
   :second       :seconds
   :seconds      :seconds
   ;; minutes
   :m            :minutes
   :min          :minutes
   :mins         :minutes
   :minute       :minutes
   :minutes      :minutes
   ;; hours
   :h            :hours
   :hr           :hours
   :hrs          :hours
   :hour         :hours
   :hours        :hours
   ;; days
   :d            :days
   :day          :days
   :days         :days})

(def ^:private compact-duration-unit->canonical
  "Units supported in compact string syntax like \"3m\" / \"0.05s\"."
  {"ns"      :nanos
   "us"      :micros
   "µs"      :micros
   "ms"      :millis
   "s"       :seconds
   "sec"     :seconds
   "secs"    :seconds
   "second"  :seconds
   "seconds" :seconds
   "m"       :minutes
   "min"     :minutes
   "mins"    :minutes
   "minute"  :minutes
   "minutes" :minutes
   "h"       :hours
   "hr"      :hours
   "hrs"     :hours
   "hour"    :hours
   "hours"   :hours
   "d"       :days
   "day"     :days
   "days"    :days})

(defn- ^:private assert-valid-duration-unit!
  [u]
  (when-not (keyword? u)
    (throw (AssertionError. (str "Duration unit must be a keyword, got: " (pr-str u)))))
  (when-not (contains? #{:nanos :micros :millis :seconds :minutes :hours :days} u)
    ;; We throw AssertionError on purpose, to mirror tick.core/new-duration style failures.
    (throw (AssertionError. (str "Unsupported duration unit: " (pr-str u))))))

(defn- ^:private canonicalize-duration-unit
  "Normalizes common aliases (like :sec -> :seconds)."
  [u]
  (when-some [u (u/some-keyword-down-tr u)]
    (or (get parse-duration-unit-alias u)
        (get parse-duration-unit-alias (get duration-map u))
        u)))

(defn- ^:private duration-from-decimal
  "Creates java.time.Duration from a BigDecimal value + canonical unit keyword."
  ^Duration [^java.math.BigDecimal n unit]
  (assert-valid-duration-unit! unit)
  (let [^java.math.BigDecimal factor
        (case unit
          :nanos   (java.math.BigDecimal. "1")
          :micros  (java.math.BigDecimal. "1000")
          :millis  (java.math.BigDecimal. "1000000")
          :seconds (java.math.BigDecimal. "1000000000")
          :minutes (java.math.BigDecimal. "60000000000")
          :hours   (java.math.BigDecimal. "3600000000000")
          :days    (java.math.BigDecimal. "86400000000000"))
        ;; nanos = n * factor, rounded to nearest nanosecond
        ^java.math.BigDecimal nanos-bd
        (.setScale (.multiply n factor) 0 java.math.RoundingMode/HALF_UP)
        nanos (try
                (.longValueExact nanos-bd)
                (catch ArithmeticException _
                  (throw (ArithmeticException.
                          (str "Duration too large (nanos overflow): " (.toPlainString nanos-bd))))))]
    (Duration/ofNanos nanos)))

(defn- ^:private parse-duration-compact-string
  "Parses compact duration syntax: \"3m\", \"10s\", \"0.05s\", \"250ms\", etc.
  Also accepts ISO-8601 duration strings like \"PT3M\" (via Duration/parse)."
  ^Duration [^String s]
  (let [s (some-> s str/trim)]
    (when (seq s)
      (cond
        ;; ISO-8601 duration
        (or (str/starts-with? s "P") (str/starts-with? s "p"))
        (Duration/parse (str/upper-case s))

        :else
        (let [m (re-matches #"(?i)^\s*([+-]?\d+(?:\.\d+)?)\s*([a-zµ]+)\s*$" s)]
          (when m
            (let [num-str  (nth m 1)
                  unit-str (-> (nth m 2) str/lower-case)
                  unit     (get compact-duration-unit->canonical unit-str)]
              (when unit
                (duration-from-decimal (java.math.BigDecimal. num-str) unit)))))))))

(defn- parse-ts-core
  [s multiply?]
  (when (u/valuable? s)
    (if (t/instant? s) s
        (if (inst? s)
          (t/instant s)
          (t/instant (if multiply?
                       (* 1000 (u/some-long s))
                       (u/some-long s)))))))

(defn parse-ts
  [s]
  (parse-ts-core s false))

(defn parse-ts-secs
  [s]
  (parse-ts-core s true))

(defn safe-parse-ts
  [s]
  (try (parse-ts-core s false)
       (catch Throwable _e nil)))

(defn safe-parse-ts-secs
  [s]
  (try (parse-ts-core s true)
       (catch Throwable _e nil)))

(def date-time-rpat #"(\d\d\d\d[\.\-\/]\d\d[\.\-\/]\d\d)\s(\d\d:\d\d:\d\d(\.\d{2,4})?)")
(def tstamp-pat     #"\d{10,17}")

(defn- parse-dt-core
  [s multiply?]
  (when (u/valuable? s)
    (if (inst? s)
      (t/instant s)
      (if (number? s)
        (parse-ts-core s multiply?)
        (if (string? s)
          (let [[_ d t] (re-find date-time-rpat s)]
            (if (and d t)
              (tp/parse (str d "T" t))
              (if-let [t (re-find tstamp-pat s)]
                (parse-ts-core t multiply?)
                (tp/parse s))))
          (tp/parse (str s)))))))

(defn parse-dt
  [s]
  (parse-dt-core s false))

(defn parse-dt-secs
  [s]
  (parse-dt-core s true))

(defn safe-parse-dt
  [s]
  (try (parse-dt-core s false)
       (catch Throwable _e nil)))

(defn safe-parse-dt-secs
  [s]
  (try (parse-dt-core s true)
       (catch Throwable _e nil)))

(defn timestamp
  (^Long [] (.toEpochMilli ^Instant (t/now)))
  (^Long [t]
   (when (u/valuable? t)
     (let [t (parse-dt t)]
       (if (u/instant? t)
         (.toEpochMilli ^Instant t)
         (.toEpochMilli ^Instant (t/instant t)))))))

(defn timestamp-secs
  (^Long []  (long (/ (timestamp)   1000)))
  (^Long [t] (when-some [t (timestamp t)] (long (/ t 1000)))))

(defn zero-duration?
  ^Boolean [^Duration d]
  (.isZero ^Duration d))

(defn pos-duration?
  ^Boolean [^Duration d]
  (not (or (.isNegative ^Duration d)
           (.isZero     ^Duration d))))

(defn neg-duration?
  ^Boolean [^Duration d]
  (.isNegative ^Duration d))

(defn parse-dur-min
  ^Duration [v]
  (when-some [v (u/some-long v)]
    (t/new-duration v :minutes)))

(defn parse-duration
  "Parses a duration from many forms.

  Supported:
  - nil -> nil
  - java.time.Duration -> passthrough
  - number -> treated as (default-unit), default is :minutes
  - string:
      * ISO-8601 Duration (\"PT3M\")
      * compact (\"3m\", \"0.05s\", \"250ms\")
  - vector/list:
      * [time unit] or [unit time]
      * even-length sequences are treated as (time unit) pairs and summed
  - map:
      * {:time <n> :unit <kw>} (unit optional -> default-unit)

  Notes:
  - Keyword shorthands like :m are intentionally NOT accepted here (they should throw),
    even if you keep a broader duration-map for convenience elsewhere."
  ([v]
   (parse-duration v :minutes))
  ([v default-unit]
   (let [default-unit (canonicalize-duration-unit default-unit)]
     (when (some? default-unit)
       ;; if you pass default-unit, we validate it early
       (assert-valid-duration-unit! default-unit))
     (cond
       (nil? v)
       nil

       (instance? Duration v)
       v

       ;; numeric -> default unit
       (number? v)
       (t/new-duration v (or default-unit :minutes))

       ;; string -> compact/ISO parser (if it can't parse, return ZERO to stay conservative)
       (string? v)
       (or (parse-duration-compact-string v)
           Duration/ZERO)

       ;; map form {:time .. :unit ..}
       (map? v)
       (let [tval (get v :time)
             uval (some-> (get v :unit) canonicalize-duration-unit)
             uval (or uval default-unit :minutes)]
         (assert-valid-duration-unit! uval)
         (t/new-duration tval uval))

       ;; sequential forms
       (sequential? v)
       (let [xs (seq v)]
         (cond
           (nil? xs)
           Duration/ZERO

           ;; single element -> treat as numeric with default-unit
           (and (next xs) (nil? (nnext xs)))
           (let [a (first xs)
                 b (second xs)]
             (cond
               (and (number? a) (keyword? b))
               (let [u (canonicalize-duration-unit b)]
                 ;; do NOT accept :m etc: if not canonical -> assert
                 (assert-valid-duration-unit! u)
                 (t/new-duration a u))

               (and (keyword? a) (number? b))
               (let [u (canonicalize-duration-unit a)]
                 (assert-valid-duration-unit! u)
                 (t/new-duration b u))

               :else
               ;; fallback: treat first element as value with default unit
               (parse-duration a default-unit)))

           ;; even-length -> sum pairs (time unit)
           (even? (count v))
           (reduce
            (fn [^Duration acc [time unit]]
              (let [u (canonicalize-duration-unit unit)]
                (assert-valid-duration-unit! u)
                (.plus acc ^Duration (t/new-duration time u))))
            Duration/ZERO
            (partition 2 v))

           :else
           (throw (IllegalArgumentException.
                   (str "Odd number of elements in duration sequence: " (pr-str v))))))

       :else
       (throw (IllegalArgumentException.
               (str "Unsupported duration value: " (pr-str (type v)) " " (pr-str v))))))))

(defn time-unit
  ([v default-unit]
   (time-unit v default-unit
              (get unit-to-efn (get duration-map default-unit default-unit))))
  ([v default-unit extraction-fn]
   (when v
     (if (t/duration? v)
       (extraction-fn v)
       (if (number? v)
         (u/safe-parse-long v)
         (when-some [dur (parse-duration v (or default-unit :minutes))]
           (extraction-fn dur)))))))

(defn millis
  ([v]         (time-unit v :millis t/millis))
  ([v default] (or (time-unit v :millis t/millis) default)))

(def milliseconds millis)

(defn seconds
  ([v]         (time-unit v :seconds t/seconds))
  ([v default] (or (time-unit v :seconds t/seconds) default)))

(defn minutes
  ([v]         (time-unit v :minutes t/minutes))
  ([v default] (or (time-unit v :minutes t/minutes) default)))

(defn hours
  ([v]         (time-unit v :hours t/hours))
  ([v default] (or (time-unit v :hours t/hours) default)))

(defn days
  ([v]         (time-unit v :days t/days))
  ([v default] (or (time-unit v :days t/days) default)))

(def one-second
  (t/new-duration 1 :seconds))

(defn add-second
  [t]
  (t/>> t one-second))

(defn sub-second
  [t]
  (t/<< t one-second))

(defn days-ago
  "Returns a time of days ago from now."
  [n]
  (t/<< (t/now) (t/new-duration n :days)))

(defn duration->time
  ([duration] (duration->time duration (t/now)))
  ([duration t] (t/<< t duration)))

(defn time->duration
  ([t] (t/duration t))
  ([from-time to-time] (t/between (t/time from-time) (t/time to-time))))

(defn timeish?
  [v]
  (or (t/instant?   v)
      (t/time?      v)
      (t/date-time? v)))

(defn duration?
  [v]
  (t/duration? v))

(defn duration-or-time
  ([v]
   (duration-or-time v nil))
  ([v t]
   (when v
     (if (t/duration? v)
       [v (duration->time v (or t (t/now)))]
       [(time->duration v (or t (t/now))) v]))))

(def utc (t/zone "UTC"))

(defn instant-utc
  ([] (instant-utc (t/now)))
  ([v]
   (if (t/instant? v)
     (t/date-time (.atZone ^Instant v ^ZoneRegion utc))
     (if (inst? v)
       (t/date-time (.atZone ^Instant (t/instant v) ^ZoneRegion utc))
       (t/date-time (.atZone ^Instant (parse-dt v) ^ZoneRegion utc))))))

(defn local-utc-delta
  []
  (.getTimeInMillis
   (doto (Calendar/getInstance)
     (.clear)
     (.set 1970 Calendar/JANUARY 1 0 0 0))))

(defn date-to-local-utc
  ^Instant [^Date t]
  (t/instant (.getTime t)))

(defn utc-to-local-date
  ^Date [t]
  (t/inst (timestamp t)))

(defn utc-instant
  ([] (t/now))
  ([v]
   (if (t/instant? v)
     v
     (if (inst? v)
       (date-to-local-utc v)
       (parse-dt v)))))

(defn try-times*
  [times thunk]
  (let [res (first (drop-while
                    u/exception?
                    (repeatedly times
                                #(try (thunk)
                                      (catch Exception e
                                        (do
                                          (println "Exception:" (str e))
                                          (Thread/sleep 5000)
                                          e))))))]
    (if (u/exception? res)
      (throw res)
      res)))

(defmacro try-times
  "Executes body. If an exception is thrown, will retry. At most n retries are done. If
  still some exception is thrown it is bubbled upwards in the call chain."
  ([& body]
   (let [n (first body)]
     (if (pos-int? n)
       `(try-times* ~n (fn [] ~@(rest body)))
       `(try-times* 25 (fn [] ~@body))))))
