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
            [io.randomseed.utils :refer :all]))

(defn- parse-ts-core
  [s multiply?]
  (if (valuable? s)
    (if (t/instant? s) s
        (if (inst? s)
          (t/instant s)
          (t/instant (if multiply?
                       (* 1000 (some-long s))
                       (some-long s)))))))

(defn parse-ts
  [s]
  (parse-ts-core s false))

(defn parse-ts-secs
  [s]
  (parse-ts-core s true))

(defn safe-parse-ts
  [s]
  (try (parse-ts-core s false)
       (catch Throwable e nil)))

(defn safe-parse-ts-secs
  [s]
  (try (parse-ts-core s true)
       (catch Throwable e nil)))

(def date-time-rpat #"(\d\d\d\d[\.\-\/]\d\d[\.\-\/]\d\d)\s(\d\d:\d\d:\d\d(\.\d{2,4})?)")
(def tstamp-pat     #"\d{10,17}")

(defn- parse-dt-core
  [s multiply?]
  (if (valuable? s)
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
       (catch Throwable e nil)))

(defn safe-parse-dt-secs
  [s]
  (try (parse-dt-core s true)
       (catch Throwable e nil)))

(defn timestamp
  ([] (.toEpochMilli (t/now)))
  ([t]
   (if (valuable? t)
     (let [t (parse-dt t)]
       (if (instant? t)
         (.toEpochMilli ^Instant t)
         (.toEpochMilli ^Instant (t/instant t)))))))

(defn timestamp-secs
  ([]  (long (/ (timestamp)   1000)))
  ([t] (if-some [t (timestamp t)] (long (/ t 1000)))))

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
  [v]
  (if-some [v (some-long v)]
    (t/new-duration v :minutes)))

(def ^:const duration-map
  {:yoctosecond  :yoctoseconds
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

(defn parse-duration
  "Parses time duration expressed as value and unit. For a single argument being a
  single value it is treated as minutes. For a single value being a collection the
  function is applied to consecutive values. For the given `d` and `default-unit` the
  value of `d` is parsed as a long number and the unit is converted into a keyword
  and should be one of the: `:day`, `:hour`, `:minute`, `:second`, `:millisecond`,
  `:microsecond` or `:nanosecond` (including their corresponding plural forms). When
  multiple arguments are given they are parsed in pairs and added to create a single
  duration."
  ([d]
   (parse-duration d :minutes))
  ([d default-unit]
   (if d
     (if (t/duration? d)
       d
       (if (map? d)
         (let [t (or (:time d) (:duration d))
               u (or (:unit d) (:units    d))]
           (if (and t u)
             (parse-duration (cons u (cons t nil)))
             (if (and (= (count d) 1) (number? (ffirst d)))
               (parse-duration (first d))
               (t/new-duration 0 (or default-unit :minutes)))))
         (if (sequential? d)
           (let [dunit (some-keyword default-unit)
                 dunit (or (get duration-map dunit dunit) :minutes)]
             (->> (partition 2 2 '(nil) d)
                  (map (fn [d]
                         (let [unit (some-keyword (nth d 1))]
                           (t/new-duration (safe-parse-long (nth d 0 0) 0)
                                           (or (and unit (get duration-map unit unit))
                                               dunit)))))
                  (reduce t/+)))
           (t/new-duration (safe-parse-long d 0)
                           (if-some [dunit (some-keyword default-unit)]
                             (get duration-map dunit dunit)
                             :minutes)))))))
  ([d default-unit & pairs]
   (parse-duration (cons d (cons default-unit pairs)) :minutes)))

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

(defn time-unit
  ([v default-unit]
   (time-unit v default-unit
              (get unit-to-efn (get duration-map default-unit default-unit))))
  ([v default-unit extraction-fn]
   (if v
     (if (t/duration? v)
       (extraction-fn v)
       (if (number? v)
         (safe-parse-long v)
         (if-some [dur (parse-duration v (or default-unit :minutes))]
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
   (if v
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

(defn local-utc
  [t]
  (t/instant (.getTime t)))

(defn utc-local
  [t]
  (t/inst (timestamp t)))

(defn utc-instant
  ([] (t/now))
  ([v]
   (if (t/instant? v)
     v
     (if (inst? v)
       (local-utc v)
       (parse-dt v)))))

(defn try-times*
  [times thunk]
  (let [res (first (drop-while
                    exception?
                    (repeatedly times
                                #(try (thunk)
                                      (catch Exception e
                                        (do
                                          (println "Exception:" (str e))
                                          (Thread/sleep 5000)
                                          e))))))]
    (if (exception? res)
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
