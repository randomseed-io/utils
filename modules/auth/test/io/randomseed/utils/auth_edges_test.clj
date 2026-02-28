(ns io.randomseed.utils.auth-edges-test
  (:require [clojure.spec.alpha :as s]
            [clojure.test :refer :all]
            [tick.core :as t]
            [io.randomseed.utils.auth :as auth]
            [io.randomseed.utils.auth.algo.append :as append]
            [io.randomseed.utils.auth.algo.fail :as fail]
            [io.randomseed.utils.auth.algo.pbkdf2 :as pbkdf2]
            [io.randomseed.utils.auth.algo.scrypt :as scrypt]
            [io.randomseed.utils.auth.locking :as locking]
            [io.randomseed.utils.auth.pwd :as pwd]
            [io.randomseed.utils.auth.specs]
            [io.randomseed.utils.auth.types :as types]))

(defn b
  [^String s]
  (.getBytes s "UTF-8"))

(deftest specs-edge-predicates-test
  (is (s/valid? :io.randomseed.utils.auth/password-chain
                [{:handler-id 'io.randomseed.utils.auth.algo.append/handler
                  :password   (b "x")}]))
  (is (not (s/valid? :io.randomseed.utils.auth/password-chain
                     [{:handler-id 'io.randomseed.utils.auth.algo.append/handler}])))
  (is (s/valid? :io.randomseed.utils.auth.password-chain/intrinsic
                [{:handler-id 'io.randomseed.utils.auth.algo.append/handler
                  :password   (b "x")}]))
  (is (not (s/valid? :io.randomseed.utils.auth.password-chain/intrinsic
                     [{}]))))

(deftest auth-settings-config-branches-test
  (with-redefs [io.randomseed.utils.var/deref (fn [_] {:default {:id :from-var}})]
    (is (= {:default {:id :from-var}} (auth/settings 'x/y)))
    (is (= {:default {:id :from-var}} (auth/settings "x/y")))
    (is (= {:default {:id :from-var}} (auth/settings #'auth/setup))))
  (is (= {:k 1} (auth/settings {:k 1})))
  (is (nil? (auth/settings nil)))
  (is (true? (auth/settings? {})))
  (is (false? (auth/settings? 1)))
  (is (= {:a 1} (auth/config {:default {:a 1}})))
  (is (= {:b 2} (auth/config {:config {:b 2}})))
  (is (= {:c 3} (auth/config {:c 3})))
  (is (= {:c 3} (auth/config {:c 3} :ignored)))
  (is (nil? (auth/config nil))))

(deftest auth-password-api-branches-test
  (let [calls   (atom [])
        checker (fn [& args] (swap! calls conj args) :checked)
        cfg     {:passwords {:check checker}}]
    (is (= :checked (auth/check-password "p" {:x 1} cfg)))
    (is (= :checked (auth/check-password "p" [1] cfg)))
    (is (= :checked (auth/check-password "p" [1] [2] cfg)))
    (is (nil? (auth/check-password nil [1] cfg)))
    (is (nil? (auth/check-password "p" nil cfg)))
    (is (= 3 (count @calls))))
  (let [calls      (atom [])
        checker-js (fn [& args] (swap! calls conj args) :checked-json)
        cfg        {:passwords {:check-json checker-js}}]
    (is (= :checked-json (auth/check-password-json "p" {:x 1} cfg)))
    (is (= :checked-json (auth/check-password-json "p" "shared" cfg)))
    (is (= :checked-json (auth/check-password-json "p" "shared" "intr" cfg)))
    (is (nil? (auth/check-password-json nil {:x 1} cfg)))
    (is (nil? (auth/check-password-json "p" nil cfg)))
    (is (= 3 (count @calls))))
  (let [cfg {:passwords {:encrypt identity :encrypt-json str}}]
    (is (= "p" (auth/make-password "p" cfg)))
    (is (= "p" (auth/make-password-json "p" cfg)))
    (is (nil? (auth/make-password nil cfg))))
  (is (map? (auth/init-auth :k {:id :x :passwords {:suite []}}))))

(deftest locking-extra-branches-test
  (let [now (t/instant "2026-02-19T10:10:00Z")]
    (is (= (t/instant "2026-02-19T10:00:00Z")
           (locking/hard-lock-time {:auth/locked-at (t/instant "2026-02-19T10:00:00Z")})))
    (is (= (t/instant "2026-02-19T10:04:00Z")
           (locking/soft-lock-time {:soft-locked (t/instant "2026-02-19T10:04:00Z")})))
    (is (= locking/*lock-wait-default* (locking/lock-wait nil)))
    (is (true? (locking/soft-locked? (t/new-duration 1 :minutes) nil)))
    (is (some? (locking/soft-lock-remains (t/new-duration 1 :minutes) (t/new-duration 3 :minutes))))
    (is (some? (locking/soft-lock-remains (t/new-duration 1 :minutes) nil)))
    (is (nil? (locking/soft-lock-remains {:soft-locked (t/instant "2026-02-19T09:50:00Z")}
                                         {:locking {:lock-wait (t/new-duration 10 :minutes)}}
                                         now)))))

(deftest algorithm-arities-and-fallbacks-test
  (is (nil? (append/parse-random nil (vec "ab"))))
  (is (map? (append/encrypt "p")))
  (is (map? (append/encrypt "p" {})))
  (is (map? (fail/encrypt "p")))
  (is (map? (fail/encrypt "p" {})))
  (is (= {:password nil} (fail/encrypt "p" 1 {})))
  (is (map? (pbkdf2/encrypt "p")))
  (is (map? (pbkdf2/encrypt "p" {:iterations 1 :algorithm "HmacSHA256" :salt (b "12345678")})))
  (is (map? (scrypt/encrypt "p")))
  (is (map? (scrypt/encrypt "p" {:cpu-cost 2 :mem-cost 1 :parallel 1 :salt (b "1234567890123456")}))))

(deftest pwd-helpers-and-json-branches-test
  (pwd/wait 0.0 0.001)
  (is (= 16 (alength (pwd/salt-bytes))))
  (is (bytes? (pwd/generate-salt 8 nil "a" "b")))
  (is (bytes? (pwd/generate-salt 4 (vec "abcd") nil nil)))
  (let [enc (:password (append/encrypt "secret" {} {}))]
    (is (true? (pwd/standard-check append/encrypt "secret" enc nil {})))
    (is (true? (pwd/standard-check append/encrypt "secret" {} enc nil {}))))
  (let [dual (types/->Suites [{:handler-id 'io.randomseed.utils.auth.algo.fail/handler}]
                             [{:handler-id 'io.randomseed.utils.auth.algo.fail/handler
                               :password   (b "x")}])]
    (is (seq (pwd/merge-suites dual)))
    (is (seq (pwd/merge-suites [{:handler-id 'io.randomseed.utils.auth.algo.fail/handler}]
                               [{:handler-id 'io.randomseed.utils.auth.algo.fail/handler
                                 :password   (b "x")}]))))
  (is (seq (pwd/merge-suites
            [{:name       :pbkdf2
              :handler    {:check-fn   identity
                           :encrypt-fn identity
                           :handler-id 'io.randomseed.utils.auth.algo.pbkdf2/handler}
              :check-fn   identity
              :encrypt-fn identity}]
            [{}])))
  (let [rt (pwd/from-json (pwd/to-json [{:password (b "x")}]))]
    (is (= 1 (count rt)))
    (is (bytes? (:password (first rt)))))
  (is (= "x" (#'pwd/bytes->base64-url-safe "x")))
  (with-redefs [io.randomseed.utils.crypto.codecs/base64-url-safe->bin (fn [_] (throw (Exception. "force-catch")))]
    (is (bytes? (#'pwd/base64-any->bin "AQID")))))

(deftest pwd-suite-and-config-branches-test
  (let [cfg {:suite [{:name        :append
                      :handler     'io.randomseed.utils.auth.algo.append/handler
                      :salt-length 4
                      :salt-prefix "pre"
                      :salt-suffix "suf"}]
             :wait 0
             :wait-nouser 0
             :wait-random [0 0]}
        settings  (pwd/init :edge cfg)
        entry     (first (:suite settings))
        encrypt-f (:encrypt-fn entry)
        check-f   (:check-fn entry)
        encrypted (:password (encrypt-f "secret" settings))
        suite     [{:handler-id 'io.randomseed.utils.auth.algo.append/handler
                    :password   (b "secret")}]
        suites    (types/->Suites suite suite)
        checker   (pwd/new-checker settings)
        jchecker  (pwd/new-json-checker settings)]
    (is (vector? (pwd/encrypt "secret" {:suite [{}]})))
    (is (true? (pwd/check "secret" suite settings)))
    (is (true? (pwd/check "secret" settings suite suite)))
    (is (true? (pwd/check "secret" suites settings)))
    (is (true? (check-f "secret" encrypted nil)))
    (is (true? (check-f "secret" {} encrypted nil)))
    (is (map? (encrypt-f "secret")))
    (is (map? (encrypt-f "secret" settings)))
    (is (string? (pwd/to-json {:password "x"})))
    (is (seq (pwd/printable-suite [{:name :append}])))
    (is (map? (pwd/shared {:handler-id 'io.randomseed.utils.auth.algo.pbkdf2/handler
                           :name       :pbkdf2
                           :iterations 1
                           :algorithm  "HmacSHA256"
                           :prefix     "a"})))
    (is (= {:iterations 42
            :handler-id 'io.randomseed.utils.auth.algo.pbkdf2/handler}
           (pwd/shared {:name       :pbkdf2
                        :check-fn   identity
                        :encrypt-fn identity
                        :handler    {:id         'io.randomseed.utils.auth.algo.pbkdf2/handler
                                     :shared     [:iterations]
                                     :check-fn   identity
                                     :encrypt-fn identity
                                     :handler-id 'io.randomseed.utils.auth.algo.pbkdf2/handler}
                        :iterations 42})))
    (is (seq (pwd/shared-suite suite)))
    (is (instance? io.randomseed.utils.auth.types.Suites (pwd/split {:handler-id 'io.randomseed.utils.auth.algo.append/handler
                                                                     :password   (b "x")})))
    (is (instance? io.randomseed.utils.auth.types.Suites (pwd/split-suite suite)))
    (is (map? (pwd/human-readable {:handler-id 'io.randomseed.utils.auth.algo.append/handler
                                   :name       :append
                                   :password   (b "x")
                                   :prefix     (b "a")
                                   :suffix     (b "b")
                                   :salt       (b "c")})))
    (is (seq (pwd/human-readable-suite suite)))
    (is (map? (pwd/prepare-settings {:wait nil :x 1})))
    (is (map? (#'pwd/process-suite cfg)))
    (is (nil? (checker "secret" nil nil)))
    (is (nil? (jchecker "secret" nil nil)))
    (is (map? (pwd/init :edge2 {:suite [{:name :fail
                                         :handler 'io.randomseed.utils.auth.algo.fail/handler}]})))
    (is (= {:k {:x 1 :wait 1 :wait-random [0 2] :wait-nouser 2}}
           (pwd/expand-settings :k {:x 1})))))
