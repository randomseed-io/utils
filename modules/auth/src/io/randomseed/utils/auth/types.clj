(ns

    ^{:doc    "Random Utilities, authentication record types."
      :author "Paweł Wilk"
      :added  "2.0.6"}

    io.randomseed.utils.auth.types

  (:import [clojure.lang Keyword ISeq]
           [java.time    Duration]))

(defrecord AuthLocking
    [^Long     max-attempts
     ^Duration lock-wait
     ^Duration fail-expires])

(defrecord AuthPasswords
    [^Keyword id
     ^ISeq    suite
     check
     check-json
     encrypt
     encrypt-json
     wait])

;; DBPassword record is to pass intrinsic suite in JSON format and shared suite ID.
;; It's used to pass data around.

(defrecord DBPassword
    [^Long   password-suite-id
     ^String password])

(defrecord PasswordData
    [^String shared
     ^String intrinsic
     ^Long   suite-id])

;; Password types

(defrecord Suites
    [shared intrinsic])

(defrecord SuitesJSON
    [^String shared
     ^String intrinsic])
