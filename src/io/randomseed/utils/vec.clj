(ns

    ^{:doc    "Random utils, vector utilities."
      :author "Paweł Wilk"
      :added  "1.0.0"}

    io.randomseed.utils.vec

  (:refer-clojure :exclude [merge split-at shuffle])

  (:require [io.randomseed.utils :refer :all])

  (:import [java.util Random Collection Collections ArrayList]))

(defn split-at
  "Splits vector at the given index."
  [idx v]
  [(subvec v 0 idx) (subvec v idx)])

(defn pairs
  "Partitions a vector into pairs."
  [coll]
  (let [input (vec coll)
        len   (unchecked-dec (count input))]
    (loop [output (transient [])
           from   (unchecked-long 0)]
      (if (>= from len)
        (persistent! output)
        (let [to (unchecked-add from 2)]
          (recur (conj! output (subvec input from to)) to))))))

(defn merge
  "Merges two vectors."
  [v1 v2]
  (if (> (count v1) (count 2))
    (into v1 v2)
    (into v2 v1)))

(defn swap
  "Swaps two elements of a vector."
  [v index-1 index-2]
  (assoc v
         index-2 (nth v index-1)
         index-1 (nth v index-2)))

(defn replace-by-last
  "Removes an element under the given index from a vector by replacing it with the
  last one. For the last element index it simply removes it."
  ^clojure.lang.IPersistentVector [^clojure.lang.IPersistentVector v ^long index]
  (if (= index (unchecked-dec (count v)))
    (pop v)
    (pop (assoc v index (peek v)))))

(defn get-rand-nth
  "Returns a random element of the given vector. When the second argument is present it
  should be an instance of random number generator used to get the random position."
  {:added "1.0.0" :tag clojure.lang.Keyword}
  ([^clojure.lang.IPersistentVector v]
   (when-not-empty v (rand-nth v)))
  ([^clojure.lang.IPersistentVector v
    ^Random rng]
   (when-not-empty v
     (if (nil? rng)
       (rand-nth v)
       (nth v (.nextInt rng (count v)))))))

(defn shuffle
  "Like clojure.core/shuffle but makes use of custom random number generator."
  ([^Collection coll]
   (clojure.core/shuffle ^Collection coll))
  ([^Collection coll ^Random rng]
   (if (nil? rng)
     (clojure.core/shuffle ^Collection coll)
     (let [^ArrayList alist (ArrayList. ^Collection coll)]
       (Collections/shuffle ^ArrayList alist ^Random rng)
       (clojure.lang.RT/vector (.toArray ^ArrayList alist))))))

(defn rand-nths
  "Returns a vector with the given number of randomly selected elements of the given
  vector. No element will be selected more than once."
  (^clojure.lang.IPersistentVector [^clojure.lang.IPersistentVector coll]
   (rand-nths coll nil nil))
  (^clojure.lang.IPersistentVector
   [^clojure.lang.IPersistentVector coll ^long num]
   (rand-nths coll num nil))
  (^clojure.lang.IPersistentVector
   [^clojure.lang.IPersistentVector coll
    num
    ^Random rng]
   (if (nil? num)
     (if (nil? rng)
       (clojure.core/shuffle coll)
       (shuffle coll ^Random rng))
     (when (pos-int? num)
       (let [coll (vec coll)
             cnt  (unchecked-long (count coll))]
         (when (pos? cnt)
           (if (= cnt 1)
             coll
             (let [num (if (> num cnt) cnt (unchecked-long num))]
               (if (= 1 num)
                 (vector (get-rand-nth coll rng))
                 (if (= num cnt)
                   (rand-nths coll nil ^Random rng)
                   (loop [todo    coll
                          output  []
                          max-idx (unchecked-dec cnt)]
                     (or (and (neg? max-idx) output)
                         (and (= num (count output)) output)
                         (let [n (get-rand-int max-idx ^Random rng)]
                           (recur (replace-by-last todo n)
                                  (conj output (nth todo n))
                                  (unchecked-dec max-idx)))))))))))))))

(defn of-strings
  "Ensures that the given collection or a single value is a non-empty vector of
  strings. Optional function can be given to skip matching input argument and leave
  it as is."
  ([coll]
   (of-strings coll (constantly false)))
  ([coll skip-if]
   (when-valuable coll
     (if (skip-if coll) coll
         (let [coll (if (coll? coll) coll (cons coll nil))
               coll (when (some? (seq coll)) (mapv (comp str some-str) coll))]
           (valuable coll))))))
