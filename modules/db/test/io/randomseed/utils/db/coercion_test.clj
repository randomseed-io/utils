(ns io.randomseed.utils.db.coercion-test
  (:require [clojure.test                    :refer [deftest is testing]]
            [io.randomseed.utils.db.coercion :as    c]
            [io.randomseed.utils.db.sql      :as    sql]
            [io.randomseed.utils.identity    :as    id]
            [phone-number.core               :as    phone]))

(defn- unique-table
  []
  (keyword (str "t" (System/nanoTime))))

(defn- cleanup-coercers!
  [dispatch-keys]
  (doseq [k dispatch-keys]
    (remove-method c/in-coercer k)
    (remove-method c/out-coercer k)))

(deftest defcoercions-registers-lisp-and-snake-variants
  (let [t        (unique-table)
        col      :sample-field
        lisp-k   (sql/colspec-kw t col)
        snake-k  (sql/make-kw-snake t col)]
    (try
      (eval `(c/defcoercions ~t ~col clojure.string/upper-case clojure.string/lower-case))
      (testing "Lisp and snake dispatches are available"
        (is (= "ABC" ((c/in-coercer lisp-k)  "abc")))
        (is (= "ABC" ((c/in-coercer snake-k) "abc")))
        (is (= "abc" ((c/out-coercer lisp-k) "ABC")))
        (is (= "abc" ((c/out-coercer snake-k) "ABC"))))
      (finally
        (cleanup-coercers! [lisp-k snake-k])))))

(deftest get-in-coercer-falls-back-to-column-only
  (let [table      (unique-table)
        col        (keyword (str "fallback-" (System/nanoTime)))
        table-col  (sql/colspec-kw table col)]
    (try
      (defmethod c/in-coercer col [_] str)
      (is (nil? (c/in-coercer table-col)))
      (is (= str (c/get-in-coercer table col)))
      (is (= "42" (c/coerce-in table col 42)))
      (finally
        (remove-method c/in-coercer col)
        (remove-method c/in-coercer table-col)))))

(deftest false-coercer-means-no-coercion
  (let [table (unique-table)
        col   (keyword (str "disabled-" (System/nanoTime)))]
    (try
      (defmethod c/in-coercer col [_] false)
      (is (false? (c/get-in-coercer table col)))
      (is (= :unchanged (c/coerce-in table col :unchanged)))
      (finally
        (remove-method c/in-coercer col)))))

(deftest macros-use-coercion-rules
  (let [t        (unique-table)
        col      :num
        lisp-k   (sql/colspec-kw t col)
        snake-k  (sql/make-kw-snake t col)]
    (try
      (eval `(c/defcoercions ~t ~col inc dec))
      (is (= 2 (eval `(c/<- ~t ~col 1))))
      (is (= [2 3] (eval `(c/<<- [~t [~col 1 2]]))))
      (is (= 1 (c/-> t col 2)))
      (finally
        (cleanup-coercers! [lisp-k snake-k])))))

(deftest q-macro-builds-expected-sql-and-coerced-params
  (let [table              :coercion-test-e2e-users
        identity-col       :identity
        phone-col          :phone-number
        identity-lisp-k    (sql/colspec-kw table identity-col)
        identity-snake-k   (sql/make-kw-snake table identity-col)
        phone-lisp-k       (sql/colspec-kw table phone-col)
        phone-snake-k      (sql/make-kw-snake table phone-col)
        identity-value     (id/of-email "Local@Domain.COM")
        phone-value        (phone/number "+48 123 123 123")
        expected-query     (str "insert into `" (sql/to-snake-simple table)
                                "` (`identity`, `phone_number`) values (?, ?)")
        expected-identity  (id/to-db* identity-value)
        expected-phone     (phone/format phone-value nil :phone-number.format/e164)]
    (try
      (eval
       `(c/defcoercions ~table
          ~identity-col id/to-db* id/of
          ~phone-col (fn [v#] (phone/format v# nil :phone-number.format/e164)) phone/number))
      (let [result (c/<q ["insert into %[t] (%(idc), %(phc)) values (?, ?)"
                          {:t table :idc identity-col :phc phone-col}]
                         [table
                          [identity-col identity-value]
                          [phone-col phone-value]])]
        (is (= expected-query (first result)))
        (is (= [expected-identity expected-phone] (vec (rest result)))))
      (finally
        (cleanup-coercers! [identity-lisp-k identity-snake-k phone-lisp-k phone-snake-k])))))
