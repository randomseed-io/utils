(ns io.randomseed.utils.db.types-test
  (:require [clojure.test               :refer [deftest is testing]]
            [next.jdbc.prepare          :as    jp]
            [io.randomseed.utils.db.types :as  dbt]
            [io.randomseed.utils.identity :as  id]
            [phone-number.core          :as    phone])
  (:import  (io.randomseed.utils.identity.types Identity)
            (com.google.i18n.phonenumbers Phonenumber$PhoneNumber)
            (java.sql PreparedStatement)
            (java.lang.reflect InvocationHandler Method Proxy)))

(defn- prepared-statement-spy
  []
  (let [calls   (atom [])
        handler (reify InvocationHandler
                  (invoke [_ _ method args]
                    (let [^Method m method
                          n (.getName m)
                          a (vec (or args []))]
                      (swap! calls conj [n a])
                      nil)))
        ps      (Proxy/newProxyInstance (or (.getContextClassLoader (Thread/currentThread))
                                            (.getClassLoader PreparedStatement))
                                        (into-array Class [PreparedStatement])
                                        handler)]
    {:calls calls
     :ps    ps}))

(defn- first-call
  [calls method-name]
  (some (fn [[n args]]
          (when (= method-name n)
            args))
        @calls))

(defn- reset-db-type-registry-state!
  []
  (reset! @#'io.randomseed.utils.db.types/registered-reader-adders [])
  (reset! @#'io.randomseed.utils.db.types/registered-setter-adders [])
  (reset! @#'io.randomseed.utils.db.types/optional-adders-autoloaded? false))

(deftest add-all-accessors-autoloads-optional-setters
  (testing "Identity and phone-number JDBC adapters are loaded by add-all-accessors"
    (let [identity-value (id/of-email "a@example.com")
          phone-value    (phone/number "+48123123123")]
      (reset-db-type-registry-state!)
      (dbt/add-all-accessors)
      (is (instance? Identity identity-value))
      (is (instance? Phonenumber$PhoneNumber phone-value))
      (is (extends? jp/SettableParameter Identity))
      (is (extends? jp/SettableParameter (class phone-value))))))

(deftest add-all-accessors-performs-db-value-conversion
  (testing "Identity value is converted via id/to-db* before JDBC binding"
    (let [v                     (id/of-email "Local@Domain.COM")
          expected-db-value     (id/to-db* v)
          {:keys [ps calls]}    (prepared-statement-spy)]
      (reset-db-type-registry-state!)
      (dbt/add-all-accessors)
      (jp/set-parameter v ps 1)
      (let [[idx bound] (first-call calls "setObject")]
        (is (= 1 idx))
        (is (= expected-db-value bound)))))
  (testing "Phone-number value is converted to E.164 before JDBC binding"
    (let [v                     (phone/number "+48 123 123 123")
          expected-db-value     (phone/format v nil :phone-number.format/e164)
          {:keys [ps calls]}    (prepared-statement-spy)]
      (reset-db-type-registry-state!)
      (dbt/add-all-accessors)
      (jp/set-parameter v ps 2)
      (let [[idx bound] (first-call calls "setString")]
        (is (= 2 idx))
        (is (= expected-db-value bound))))))
