(ns io.randomseed.utils.identity-test

  (:require [clojure.test                    :refer [deftest is testing use-fixtures]]
            [clojure.test.check.clojure-test :refer [defspec]]
            [clojure.test.check.generators   :as gen]
            [clojure.test.check.properties   :as prop]
            [io.randomseed.utils.identity    :as identity]
            [io.randomseed.utils.identity.proto :as p]
            [io.randomseed.utils.identity.types :as it])

  (:import  (java.util UUID)
            (io.randomseed.utils.identity.types Identity)))

(defn- with-proto-state
  [f]
  (let [hierarchy p/type-hierarchy
        valid     p/valid-types
        matchers  p/type-string-matchers
        matcher   p/type-string-match]
    (try
      (f)
      (finally
        (alter-var-root #'p/type-hierarchy (constantly hierarchy))
        (alter-var-root #'p/valid-types (constantly valid))
        (alter-var-root #'p/type-string-matchers (constantly matchers))
        (alter-var-root #'p/type-string-match (constantly matcher))))))

(use-fixtures :each with-proto-state)

(deftest preparsers-and-constructors
  (testing "email preparse keeps local-part and lowercases domain-part"
    (is (= "Local@domain.com"
           (identity/preparse-email "Local@Domain.COM")))
    (is (nil? (identity/preparse-email "invalid-email"))))

  (testing "id preparse accepts only positive ints"
    (is (= 42 (identity/preparse-id "42")))
    (is (nil? (identity/preparse-id 0)))
    (is (nil? (identity/preparse-id -7))))

  (testing "uid preparse parses UUID values"
    (let [u-str "550e8400-e29b-41d4-a716-446655440000"]
      (is (instance? UUID (identity/preparse-uid u-str)))
      (is (= u-str (str (identity/preparse-uid u-str))))))

  (testing "constructor helpers return Identity records on valid input"
    (is (= :id (.id-type ^Identity (identity/of-id 42))))
    (is (= :email (.id-type ^Identity (identity/of-email "A@B.com"))))
    (is (= :uid (.id-type ^Identity (identity/of-uid "550e8400-e29b-41d4-a716-446655440000"))))))

(deftest type-and-acceptance-contract
  (is (identity/type? :id))
  (is (not (identity/type? :unknown)))
  (is (= :email (identity/check-type :email)))
  (is (nil? (identity/check-type :unknown)))
  (is (= :id (identity/acceptable-type 42 :io.randomseed.utils.identity/valid)))
  (is (nil? (identity/acceptable-type 42 :email))))

(deftest parser-dispatch-and-parse-single
  (let [id-parser (identity/parser :id)
        parsed    (id-parser "42")]
    (is (fn? id-parser))
    (is (instance? Identity parsed))
    (is (= :id (.id-type ^Identity parsed)))
    (is (= 42 (.value ^Identity parsed)))
    (is (nil? ((identity/parser :default) "42")))
    (is (= :id (.id-type ^Identity (identity/parse-single :id "42"))))))

(deftest number-record-map-and-seq-paths
  (let [id-record (identity/of 42)]
    (testing "number and record paths"
      (is (instance? Identity id-record))
      (is (= :id (.id-type ^Identity id-record)))
      (is (= 42 (identity/value id-record)))
      (is (= :id (identity/type id-record)))
      (is (identity/of-known-type? id-record))
      (is (identity/of-known-type? 42))
      (is (false? (identity/of-known-type? nil))))

    (testing "map path parses known identity keys"
      (let [from-map (identity/of {:user/id 7})]
        (is (instance? Identity from-map))
        (is (= :id (.id-type ^Identity from-map)))
        (is (= 7 (.value ^Identity from-map)))
        (is (nil? (identity/of {:user/id {:id 7}})))))

    (testing "sequence helpers"
      (let [parsed (vec (identity/of-seq [1 nil false]))
            some-ids (vec (identity/some-seq [1 nil false]))]
        (is (= [:id nil nil]
               (mapv #(when % (.id-type ^Identity %)) parsed)))
        (is (= 1 (count some-ids)))
        (is (= 1 (.value ^Identity (first some-ids))))))))

(deftest string-and-db-conversions
  (let [id-record (identity/of 42)]
    (is (= "42" (identity/to-str id-record)))
    (is (= "42" (identity/to-str :id 42)))
    (is (= 42 (identity/to-db id-record)))
    (is (= 42 (identity/to-db :id 42)))
    (is (= "42" (identity/->str 42)))
    (is (= 42 (identity/->db 42)))
    (is (nil? (identity/->str nil)))))

(deftest identity-string-protocol-hook
  (let [x (it/->Identity :id 123)]
    (is (= "123" (str x)))
    (is (= "123" (it/-identity-str x)))))

(deftest string-paths-and-detection
  (let [email "Local@Domain.COM"
        id-str "42"
        uid-str "550e8400-e29b-41d4-a716-446655440000"
        phone "+48123123123"
        email-id (identity/of email)
        id-id    (identity/of id-str)
        uid-id   (identity/of uid-str)
        phone-id (identity/of phone)]
    (testing "matcher chain detects known string formats"
      (is (= :email (p/type-string-match email)))
      (is (= :id    (p/type-string-match id-str)))
      (is (= :uid   (p/type-string-match uid-str)))
      (is (= :phone (p/type-string-match phone)))
      (is (nil?     (p/type-string-match "42" :email)))
      (is (= :id    (p/type-string-match "42" :id))))

    (testing "public type/of/to-str API works for string literals"
      (is (= :email (identity/type email)))
      (is (= :id    (identity/type id-str)))
      (is (= :uid   (identity/type uid-str)))
      (is (= :phone (identity/type phone)))

      (is (= :email (.id-type ^Identity email-id)))
      (is (= :id    (.id-type ^Identity id-id)))
      (is (= :uid   (.id-type ^Identity uid-id)))
      (is (= :phone (.id-type ^Identity phone-id)))

      (is (= "Local@domain.com" (identity/to-str email-id)))
      (is (= "42"               (identity/to-str id-id)))
      (is (= uid-str            (identity/to-str uid-id)))
      (is (= phone              (identity/to-str phone-id)))
      (is (= "Local@domain.com" (identity/to-db email-id)))
      (is (= 42                 (identity/to-db id-id))))

    (testing "explicit typed parsing on strings"
      (is (= :id (.id-type ^Identity (identity/of-type :id "42"))))
      (is (= :email (.id-type ^Identity (identity/of-type :email email))))
      (is (nil? (identity/of-type :email "42"))))

    (testing "sequence parsing keeps recognized string identities"
      (let [parsed   (vec (identity/of-seq [1 "2" nil :x]))
            some-ids (vec (identity/some-seq [1 "2" nil :x]))]
        (is (= [:id :id nil nil]
               (mapv #(when % (.id-type ^Identity %)) parsed)))
        (is (= [1 2]
               (mapv #(.value ^Identity %) some-ids)))))))

(deftest acceptable-type-group-lifecycle
  (let [tag :io.randomseed.utils.identity.test/id-only]
    (identity/add-acceptable-type! tag :id)
    (is (isa? p/type-hierarchy :id tag))
    (is (= :id (identity/acceptable-type 42 tag)))
    (is (= :id (.id-type ^Identity (identity/of-type tag 42))))
    (identity/unaccept-type! tag :id)
    (is (not (isa? p/type-hierarchy :id tag)))))

(defspec number-roundtrip-contract
  120
  (prop/for-all [n (gen/choose 1 1000000)]
  (let [id (identity/of n)]
      (and (instance? Identity id)
           (= :id (.id-type ^Identity id))
           (= (long n) (.value ^Identity id))
           (= (str n) (identity/to-str id))
           (= (long n) (identity/to-db id))
           (identity/of-known-type? id)))))

(defspec numeric-string-roundtrip-contract
  120
  (prop/for-all [n (gen/choose 1 1000000)]
    (let [s  (str n)
          id (identity/of s)]
      (and (instance? Identity id)
           (= :id (.id-type ^Identity id))
           (= (long n) (.value ^Identity id))
           (= s (identity/to-str id))
           (= (long n) (identity/to-db id))
           (= :id (identity/type s))))))
