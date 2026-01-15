(ns

    ^{:doc    "Random utils, validators."
      :author "Paweł Wilk"
      :added  "1.0.0"}

    io.randomseed.utils.validators

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
  (valid? [validator v] (identical? validator (keyword v)))

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
      (let [required-params (if (set? required-params) required-params (set required-params))]
        (reduce-kv (fn [_ k _]
                     (and (contains? required-params k) (reduced true)))
                   false m))))

(defn has-all-required?
  ^Boolean [required-params m]
  (or (nil? m)
      (zero? (count m))
      (reduce (fn [_ k]
                (or (contains? m k) (reduced false)))
              true required-params)))

(defn has-n-required?
  ^Boolean [min-required required-params m]
  (or (nil? m)
      (not (pos? min-required))
      (let [cm (count m)]
        (or (zero? cm)
            (and (>= cm min-required)
                 (true? (reduce (fn [num k]
                                  (if (contains? m k)
                                    (let [num (unchecked-inc-int num)]
                                      (if (= num min-required) (reduced true) num))
                                    num))
                                (unchecked-int 0) required-params)))))))

(defn validate
  ([] true)
  ([m vmap]
   (validate m vmap false nil nil))
  ([m vmap default-pass?]
   (validate m vmap default-pass? nil nil))
  ([m vmap default-pass? required-params]
   (validate m vmap default-pass? required-params nil))
  ([m vmap default-pass? required-params required-check-fn]
   (and (or (nil? required-params)
            (let [required-check-fn (or required-check-fn has-all-required?)]
              (or (required-check-fn required-params m)
                  (do (log/dbg "No required parameter was found") false))))
        (loop [items (seq m)]
          (if items
            (let [[k v] (first items)]
              (and (if (contains? vmap k)
                     (or (valid? (get vmap k) v) (do (log/dbg "Invalid parameter" k) false))
                     (or default-pass?           (do (log/dbg "Unknown parameter" k) false)))
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

(defn- first-missing-parameter
  [required-params m]
  (loop [items (seq required-params)]
    (if items
      (let [k    (first items)
            rest (next items)]
        (if (contains? m k)
          (recur rest)
          [:parameter/missing k rest])))))

(defn explain-required
  [required-params m]
  (if-not (has-required? required-params m)
    (map #(vector :parameter/missing %) required-params)))

(defn explain-all-required
  [required-params m]
  (if (some? m)
    (lazy-seq
     (if-some [f (first-missing-parameter required-params m)]
       (let [[reason k rest] f]
         (cons [reason k] (explain-all-required rest m)))))))

(defn explain-n-required
  [min-required required-params m]
  (let [min-required (min min-required (count required-params))]
    (if (and (some? m) (pos? min-required))
      (loop [items       (seq required-params)
             to-be-found (unchecked-int min-required)
             to-report   nil]
        (if (pos? to-be-found)
          (if items
            (let [k    (first items)
                  rest (next items)]
              (if (contains? m k)
                (recur rest (unchecked-dec-int to-be-found) to-report)
                (recur rest to-be-found (cons [:parameter/missing k] to-report))))
            (seq to-report)))))))

(defn validate-parameters
  [m vmap default-pass?]
  (lazy-seq
   (if-some [f (first-bad-parameter m vmap default-pass?)]
     (let [[reason k rest] f]
       (cons [reason k] (validate-parameters rest vmap default-pass?))))))

(defn explain
  ([] nil)
  ([m vmap]
   (explain m vmap false nil nil))
  ([m vmap default-pass?]
   (explain m vmap default-pass? nil nil))
  ([m vmap default-pass? required-params]
   (explain m vmap default-pass? required-params nil))
  ([m vmap default-pass? required-params required-check-fn]
   (let [reasons (validate-parameters m vmap default-pass?)
         reasons (if (nil? required-params) reasons
                     (let [required-check-fn (or required-check-fn explain-all-required)]
                       (concat (required-check-fn required-params m) reasons)))]
     (if (first reasons) reasons))))
