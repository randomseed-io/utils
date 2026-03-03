(ns

    ^{:doc    "Random Utilities, authentication record types."
      :author "Paweł Wilk"
      :added  "2.0.6"}

    io.randomseed.utils.auth.types

  (:import [clojure.lang Keyword ISeq]
           [java.time    Duration]))

(defrecord ^{:doc "Authentication locking configuration: maximum failed attempts, lock-wait duration,
  and per-failure expiration window."}
  AuthLocking
  [^Long     max-attempts
   ^Duration lock-wait
   ^Duration fail-expires])

(defrecord ^{:doc "Holds password authentication functions: encryption suite identifier, checker
  and encryptor closures (plain and JSON variants), and an optional wait function."}
  AuthPasswords
  [^Keyword id
   ^ISeq    suite
   check
   check-json
   encrypt
   encrypt-json
   wait])

(defrecord ^{:doc "Carries a password suite ID and the intrinsic suite as a JSON string, used to
  pass password data between layers (e.g. to/from database)."}
  DBPassword
  [^Long   password-suite-id
   ^String password])

(defrecord ^{:doc "Aggregates shared and intrinsic suite JSON strings together with the suite ID."}
  PasswordData
  [^String shared
   ^String intrinsic
   ^Long   suite-id])

;; Password types

(defrecord ^{:doc "Pair of password suite chains: `shared` (public parameters) and `intrinsic`
  (secret, per-user data)."}
  Suites
  [shared intrinsic])

(defrecord ^{:doc "JSON-serialized variant of `Suites`: shared and intrinsic chains as strings."}
  SuitesJSON
  [^String shared
   ^String intrinsic])
