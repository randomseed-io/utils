(ns io.randomseed.utils.time-test

  (:require
   [clojure.test :refer [deftest testing is]]
   [clojure.walk :as walk]
   [io.randomseed.utils.time :as time])

  (:import
   (java.time Instant LocalDateTime LocalTime Year ZonedDateTime Duration)
   (java.util Date)))

(defn- instant-ms ^Instant [^long ms]
  (Instant/ofEpochMilli ms))

(defn- instant-s ^Instant [^long s]
  (Instant/ofEpochSecond s))

(defn- dur-ms ^Duration [^long ms]
  (Duration/ofMillis ms))

(defn- dur-s ^Duration [^long s]
  (Duration/ofSeconds s))

(defn- dur-min ^Duration [^long m]
  (Duration/ofMinutes m))

(defn- dur-h ^Duration [^long h]
  (Duration/ofHours h))

(defn- dur-d ^Duration [^long d]
  (Duration/ofDays d))

(deftest parse-ts-and-secs
  (testing "parse-ts: millis since epoch"
    (is (= (instant-ms 1234) (time/parse-ts 1234)))
    (is (= (instant-ms 1234) (time/parse-ts "1234"))))

  (testing "parse-ts-secs: seconds since epoch"
    (is (= (instant-ms 2000) (time/parse-ts-secs 2)))
    (is (= (instant-ms 2000) (time/parse-ts-secs "2"))))

  (testing "safe-parse-ts returns nil on bad input"
    (is (nil? (time/safe-parse-ts "nope")))
    (is (nil? (time/safe-parse-ts nil)))))

(deftest parse-dt-and-secs
  (testing "parse-dt: numbers are treated as epoch millis"
    (is (= (instant-ms 1234) (time/parse-dt 1234))))

  (testing "parse-dt: 10+ digit strings are treated as epoch millis (not seconds)"
    ;; parse-dt checks 10..17 digits and routes to parse-ts-core without multiply
    (is (= (instant-ms 1234) (time/parse-dt "0000001234"))))

  (testing "parse-dt: short numeric strings are NOT treated as timestamps (tick.parse semantics)"
    ;; tick.parse tends to interpret short numeric strings as a Year (e.g. \"1234\")
    (is (= (Year/of 1234) (time/parse-dt "1234")))
    ;; and even \"2\" tends to parse as LocalTime 02:00 in tick.parse
    (is (= (LocalTime/parse "02:00") (time/parse-dt "2"))))

  (testing "parse-dt parses space-separated date-time via regex bridge -> LocalDateTime"
    (is (= (LocalDateTime/parse "2020-01-02T03:04:05")
           (time/parse-dt "2020-01-02 03:04:05"))))

  (testing "parse-dt ignores trailing Z in the space-separated regex path (current behavior)"
    ;; because date-time-rpat does not include zone, the regex strips it implicitly
    (is (= (LocalDateTime/parse "2020-01-02T03:04:05")
           (time/parse-dt "2020-01-02 03:04:05Z"))))

  (testing "parse-dt-secs multiplies numeric timestamps by 1000 (seconds -> millis)"
    (is (= (instant-ms 2000) (time/parse-dt-secs 2)))
    ;; 10 digits triggers tstamp path; multiply=true -> seconds -> millis
    (is (= (instant-ms 2000) (time/parse-dt-secs "0000000002"))))

  (testing "safe-parse-dt returns nil on bad input"
    (is (nil? (time/safe-parse-dt "nope")))
    (is (nil? (time/safe-parse-dt nil)))))

(deftest timestamp-basics
  (testing "timestamp of now is a sane epoch millis"
    (let [ts (time/timestamp)]
      (is (integer? ts))
      (is (<= 0 ts))))

  (testing "timestamp on instant"
    (is (= 1234 (time/timestamp (instant-ms 1234))))
    (is (= 2 (time/timestamp-secs (instant-ms 2999))))) ; floor millis/1000

  (testing "timestamp on numeric-like strings: use 10+ digits if you mean epoch millis"
    (is (= 1234 (time/timestamp "0000001234"))))

  (testing "timestamp on short numeric strings can throw (because parse-dt may return non-instant types)"
    (is (thrown? IllegalArgumentException (time/timestamp "1234")))))

(deftest parse-duration-basics
  (testing "nil gives nil"
    (is (nil? (time/parse-duration nil))))

  (testing "duration passes through"
    (let [d (tick.core/new-duration 3 :minutes)]
      (is (= d (time/parse-duration d)))))

  (testing "single value defaults to minutes"
    (is (= (tick.core/new-duration 3 :minutes)
           (time/parse-duration 3))))

  (testing "string form: compact unit syntax is supported"
    (is (= (tick.core/new-duration 3 :minutes)
           (time/parse-duration "3m")))
    (is (= (tick.core/new-duration 10 :seconds)
           (time/parse-duration "10s")))
    ;; 0.05s = 50ms
    (is (= (java.time.Duration/ofMillis 50)
           (time/parse-duration "0.05s"))))

  (testing "sequential pairs are added"
    (is (= (.plus (tick.core/new-duration 3 :minutes)
                  (tick.core/new-duration 10 :seconds))
           (time/parse-duration [3 :minutes 10 :seconds]))))

  (testing "map form {:time .. :unit ..} works"
    (is (= (tick.core/new-duration 3 :minutes)
           (time/parse-duration {:time 3 :unit :minutes})))
    ;; alias w mapie też może działać, jeśli chcesz:
    (is (= (tick.core/new-duration 3 :minutes)
           (time/parse-duration {:time 3 :unit :m}))))

  (testing "missing unit falls back to minutes (or provided default-unit)"
    (is (= (tick.core/new-duration 3 :minutes)
           (time/parse-duration [3 nil])))
    (is (= (tick.core/new-duration 3 :minutes)
           (time/parse-duration {:time 3})))
    (is (= (tick.core/new-duration 3 :seconds)
           (time/parse-duration {:time 3} :seconds))))

  (testing "unknown unit throws"
    (is (thrown? AssertionError
                 (time/parse-duration [3 :wut] :minutes)))
    (is (thrown? AssertionError
                 (time/parse-duration {:time 3 :unit :wut} :minutes)))))

(deftest add-sub-second
  (testing "add-second/sub-second shift instants by exactly 1s"
    (is (= (instant-s 1) (time/add-second (instant-s 0))))
    (is (= (instant-s 0) (time/sub-second (instant-s 1))))))

(deftest duration-and-time-conversions
  (testing "duration->time subtracts duration from time"
    (is (= (instant-s 90)
           (time/duration->time (dur-s 10) (instant-s 100)))))

  (testing "time->duration produces duration between times"
    (is (= (dur-s 10)
           (time/time->duration (instant-s 90) (instant-s 100))))))

(deftest duration-or-time
  (testing "duration input -> [duration time]"
    (let [base (instant-s 100)
          d    (dur-s 10)
          [d* t*] (time/duration-or-time d base)]
      (is (= d d*))
      (is (= (instant-s 90) t*))))

  (testing "time input -> [duration time]"
    (let [base (instant-s 100)
          t    (instant-s 90)
          [d* t*] (time/duration-or-time t base)]
      (is (= (dur-s 10) d*))
      (is (= t t*)))))

(deftest utc-and-date-bridges
  (testing "instant-utc returns a UTC date-time for an Instant (current behavior: LocalDateTime)"
    (let [ldt (time/instant-utc (java.time.Instant/ofEpochMilli 0))]
      (is (instance? java.time.LocalDateTime ldt))))

  (testing "date-to-local-utc converts Date -> Instant via millis"
    (let [d (java.util.Date. 1234)
          i (time/date-to-local-utc d)]
      (is (instance? java.time.Instant i))
      (is (= 1234 (.toEpochMilli ^java.time.Instant i)))))

  (testing "utc-instant passes through Instant and converts Date"
    (is (= (java.time.Instant/ofEpochMilli 0)
           (time/utc-instant (java.time.Instant/ofEpochMilli 0))))
    (is (= (java.time.Instant/ofEpochMilli 1234)
           (time/utc-instant (java.util.Date. 1234)))))

  (testing "utc-to-local-date currently throws (tick.core/inst doesn't accept millis in this tick version)"
    (is (thrown? IllegalArgumentException
                 (time/utc-to-local-date (java.time.Instant/ofEpochMilli 0))))))

(deftest local-utc-delta-basic
  (testing "local-utc-delta returns a plausible timezone offset (ms) relative to epoch"
    (let [d (time/local-utc-delta)]
      (is (integer? d))
      ;; sanity bound: timezone offsets are within +/- 14h
      (is (<= (Math/abs (long d)) (* 14 60 60 1000))))))

(deftest try-times-star-retries-without-sleep
  (testing "retries until thunk returns non-exception value (no Thread/sleep needed if we return exceptions, not throw)"
    (let [calls (atom 0)
          res (time/try-times* 5
                               (fn []
                                 (swap! calls inc)
                                 (if (< @calls 3)
                                   (ex-info "nope" {})
                                   :ok)))]
      (is (= 3 @calls))
      (is (= :ok res))))

  (testing "if thunk keeps returning Exception objects, function returns nil (documents current behavior)"
    (is (nil? (time/try-times* 3 (fn [] (ex-info "still nope" {})))))))

(deftest try-times-macroexpansion
  (testing "when first form is pos-int, it is treated as retry count"
    (let [ex (macroexpand '(io.randomseed.utils.time/try-times 3 (inc 1)))]
      (is (= 'io.randomseed.utils.time/try-times* (first ex)))
      (is (= 3 (second ex)))
      (is (= 'clojure.core/fn (first (nth ex 2))))
      (is (= '(inc 1) (last (nth ex 2))))))

  (testing "otherwise defaults to 25 and wraps whole body"
    (let [ex (macroexpand '(io.randomseed.utils.time/try-times (inc 1)))]
      (is (= 'io.randomseed.utils.time/try-times* (first ex)))
      (is (= 25 (second ex)))
      (is (= 'clojure.core/fn (first (nth ex 2))))
      (is (= '(inc 1) (last (nth ex 2)))))))

(deftest time-unit-and-convenience-fns
  (testing "tick-style units work"
    (is (= (tick.core/new-duration 3 :minutes)
           (time/parse-duration [3 :minutes])))
    (is (= (tick.core/new-duration 10 :seconds)
           (time/parse-duration [10 :seconds]))))

  (testing "shorthand units like :m are accepted (alias layer before tick)"
    (is (= (tick.core/new-duration 3 :minutes)
           (time/parse-duration [3 :m])))))
