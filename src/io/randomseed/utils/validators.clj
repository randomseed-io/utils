(ns

    ^{:doc    "Random utils, validators."
      :author "Paweł Wilk"
      :added  "1.0.0"}

    io.randomseed.utils.validators

  (:refer-clojure :exclude [parse-long uuid random-uuid])

  (:require [clojure.spec.alpha      :as       s]
            [io.randomseed.utils.map :as     map]
            [io.randomseed.utils.log :as     log]
            [io.randomseed.utils.var :as     var]
            [io.randomseed.utils     :refer :all])

  (:import [java.util.regex Pattern]
           [clojure.spec.alpha Spec]))

(defprotocol Validating
  (valid? [validator v]))

(extend-protocol Validating
  clojure.lang.PersistentArrayMap
  clojure.lang.PersistentHashMap
  (valid? [validator v] (boolean (get validator v)))

  clojure.lang.PersistentHashSet
  (valid? [validator v] (contains? validator v))

  clojure.lang.Keyword
  (valid? [validator v] (= validator (keyword v)))

  Spec
  (valid? [validator v] (s/valid? validator v))

  Pattern
  (valid? [validator v]
    (if v
      (if-some [m (re-matches validator v)]
        (if (coll? m)
          (some? (first m))
          (some? m))
        false)
      false))

  String
  (valid? [validator v] (= validator v))

  Number
  (valid? [validator v]
    (if v
      (if (number? v)
        (= validator v)
        (or (= v (str validator))
            (= validator (safe-parse-num v))))
      false))

  clojure.lang.Fn
  (valid? [validator v] (boolean (validator v)))

  Boolean
  (valid? [validator v] validator)

  nil
  (valid? [validator v] false))

(defn has-required?
  ^Boolean [required-params m]
  (or (nil? m)
      (zero? (count m))
      (reduce-kv (fn [_ k _]
                   (and (contains? required-params k) (reduced true)))
                 false m)))

(defn validate
  ([] true)
  ([m vmap]
   (validate m vmap false nil))
  ([m vmap default-pass?]
   (validate m vmap default-pass? nil))
  ([m vmap default-pass? required-params]
   (and (or (nil? required-params)
            (or (has-required? required-params m)
                (do (log/msg "No required parameter was found") false)))
        (loop [items (seq m)]
          (if items
            (let [[k v] (first items)]
              (and (if (contains? vmap k)
                     (or (valid? (get vmap k) v) (do (log/msg "Invalid parameter" k) false))
                     (or default-pass?           (do (log/msg "Unknown parameter" k) false)))
                   (recur (next items))))
            true)))))

(defn- first-bad-parameter
  [m vmap default-pass?]
  (loop [items (seq m)]
    (if items
      (let [[k v] (first items)
            rest  (next items)]
        (if (contains? vmap k)
          (if (valid? (get vmap k) v) (recur rest) [:parameter/invalid k rest])
          (if default-pass?           (recur rest) [:parameter/unknown k rest]))))))

(defn validate-parameters
  [m vmap default-pass?]
  (lazy-seq
   (if-some [f (first-bad-parameter m vmap default-pass?)]
     (let [[reason k rest] f]
       (cons [reason k] (validate-parameters rest vmap default-pass?))))))

(defn explain
  ([] nil)
  ([m vmap]
   (explain m vmap false nil))
  ([m vmap default-pass?]
   (explain m vmap default-pass? nil))
  ([m vmap default-pass? required-params]
   (let [reasons (validate-parameters m vmap default-pass?)]
     (if (nil? required-params)
       reasons
       (lazy-seq
        (when (has-required? required-params m)
          (cons [:parameter/missing-of required-params] reasons)))))))
