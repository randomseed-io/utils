(ns io.randomseed.utils.auth-test
  (:require [clojure.test :refer :all]
            [io.randomseed.utils.auth :as auth]
            [io.randomseed.utils.auth.pwd :as pwd]))

(def pbkdf2-scrypt-suite
  [{:name :pbkdf2
    :handler 'io.randomseed.utils.auth.algo.pbkdf2/handler}
   {:name :scrypt
    :handler 'io.randomseed.utils.auth.algo.scrypt/handler}])

(defn make-auth-config-for-suite
  [suite]
  (auth/make-auth
   {:id        :default
    :passwords (auth/init-passwords :default {:suite suite})}))

(defn make-auth-config
  []
  (make-auth-config-for-suite pbkdf2-scrypt-suite))

(defn make-settings-config
  []
  {:config {:id        :default
            :passwords (auth/init-passwords
                        :default
                        {:suite pbkdf2-scrypt-suite})}})

(deftest auth-config-shape-test
  (let [auth-config (make-auth-config)
        passwords   (:passwords auth-config)
        locking     (:locking auth-config)
        suite-entry (first (:suite passwords))]
    (is (map? auth-config))
    (is (= :default (:id auth-config)))
    (is (map? passwords))
    (is (sequential? (:suite passwords)))
    (is (ifn? (:check passwords)))
    (is (ifn? (:check-json passwords)))
    (is (ifn? (:encrypt passwords)))
    (is (ifn? (:encrypt-json passwords)))
    (is (ifn? (:wait passwords)))
    (is (map? suite-entry))
    (is (ifn? (:encrypt-fn suite-entry)))
    (is (ifn? (:check-fn suite-entry)))
    (is (map? locking))
    (is (number? (:max-attempts locking)))
    (is (instance? java.time.Duration (:lock-wait locking)))
    (is (instance? java.time.Duration (:fail-expires locking)))))

(deftest auth-init-config-shape-test
  (let [settings     (auth/init-config (make-settings-config))
        auth-config  (:default settings)
        passwords    (:passwords auth-config)
        locking      (:locking auth-config)]
    (is (map? settings))
    (is (map? auth-config))
    (is (= :default (:id auth-config)))
    (is (map? passwords))
    (is (sequential? (:suite passwords)))
    (is (ifn? (:check passwords)))
    (is (ifn? (:encrypt passwords)))
    (is (map? locking))
    (is (number? (:max-attempts locking)))
    (is (instance? java.time.Duration (:lock-wait locking)))
    (is (instance? java.time.Duration (:fail-expires locking)))))

(deftest password-roundtrip-test
  (let [auth-config (make-auth-config)
        suites      (auth/make-password "s3kret" auth-config)]
    (is (some? suites))
    (is (true? (auth/check-password "s3kret" suites auth-config)))
    (is (false? (auth/check-password "wrong" suites auth-config)))))

(deftest password-json-roundtrip-test
  (let [auth-config (make-auth-config)
        suites-json (auth/make-password-json "s3kret" auth-config)]
    (is (some? suites-json))
    (is (true? (auth/check-password-json "s3kret" suites-json auth-config)))
    (is (false? (auth/check-password-json "wrong" suites-json auth-config)))))

(deftest password-json-read-roundtrip-test
  (let [auth-config   (make-auth-config)
        suites-json   (auth/make-password-json "s3kret" auth-config)
        shared-suite  (pwd/from-json (:shared suites-json))
        user-suite    (pwd/from-json (:intrinsic suites-json))]
    (is (vector? shared-suite))
    (is (vector? user-suite))
    (is (true? (auth/check-password "s3kret" shared-suite user-suite auth-config)))
    (is (false? (auth/check-password "wrong" shared-suite user-suite auth-config)))))

(deftest append-handler-roundtrip-test
  (let [auth-config (make-auth-config-for-suite
                     [{:name    :append
                       :handler 'io.randomseed.utils.auth.algo.append/handler
                       :prefix  "pre-{{RND 8}}"
                       :suffix  "{{RND}}-post"}])
        suites      (auth/make-password "s3kret" auth-config)
        suites-json (auth/make-password-json "s3kret" auth-config)]
    (is (some? suites))
    (is (true? (auth/check-password "s3kret" suites auth-config)))
    (is (false? (auth/check-password "wrong" suites auth-config)))
    (is (some? suites-json))
    (is (true? (auth/check-password-json "s3kret" suites-json auth-config)))
    (is (false? (auth/check-password-json "wrong" suites-json auth-config)))))

(deftest fail-handler-always-fails-test
  (let [auth-config (make-auth-config-for-suite
                     [{:name    :fail
                       :handler 'io.randomseed.utils.auth.algo.fail/handler}])
        suites      (auth/make-password "s3kret" auth-config)]
    (is (some? suites))
    (is (false? (auth/check-password "s3kret" suites auth-config)))
    (is (false? (auth/check-password "wrong" suites auth-config)))))
