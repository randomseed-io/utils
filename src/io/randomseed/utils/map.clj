(ns

    ^{:doc    "Random utils, map utilities."
      :author "Paweł Wilk"
      :added  "1.0.0"}

    io.randomseed.utils.map

  (:refer-clojure :exclude [parse-long uuid random-uuid])

  (:require [io.randomseed.utils :refer  :all]
            [lazy-map.core       :as lazy-map])

  (:import  [lazy_map.core LazyMapEntry LazyMap]))

(defmacro lazy-get
  "Like get but the default value is an expression that is going to be evaluated only
  if the associative structure does not contain the given key."
  [m k exp]
  `(let [m# ~m, k# ~k]
     (if (and (associative? m#) (contains? m# k#)) (get m# k#) ~exp)))

(defn or-map
  "Returns an empty map if the argument is `nil`. Otherwise returns its argument."
  [m]
  (if (nil? m) {} m))

(defn- jassoc
  ^clojure.lang.Associative [^clojure.lang.Associative m k v]
  (.assoc m k v))

(defn qassoc
  "Faster version of `assoc` with some of the checks and conversions
  disabled. Associates key `a` with value `b` in `mp`. If `mp` is `nil` it creates a
  new map."
  (^clojure.lang.Associative [^clojure.lang.Associative mp a b]
   (if (nil? mp)
     {a b}
     (.assoc mp a b)))
  (^clojure.lang.Associative [^clojure.lang.Associative mp a b c d]
   (if (nil? mp)
     {a b c d}
     (-> (.assoc mp a b)
         (jassoc c d))))
  (^clojure.lang.Associative [^clojure.lang.Associative mp a b c d e f]
   (if (nil? mp)
     {a b c d e f}
     (-> (.assoc mp a b)
         (jassoc c d)
         (jassoc e f))))
  (^clojure.lang.Associative [^clojure.lang.Associative mp a b c d e f g h]
   (if (nil? mp)
     {a b c d e f g h}
     (-> (.assoc mp a b)
         (jassoc c d)
         (jassoc e f)
         (jassoc g h))))
  (^clojure.lang.Associative [^clojure.lang.Associative mp a b c d e f g h i j]
   (if (nil? mp)
     {a b c d e f g h i j}
     (-> (.assoc mp a b)
         (jassoc c d)
         (jassoc e f)
         (jassoc g h)
         (jassoc i j))))
  (^clojure.lang.Associative [^clojure.lang.Associative mp a b c d e f g h i j k l]
   (if (nil? mp)
     {a b c d e f g h i j k l}
     (-> (.assoc mp a b)
         (jassoc c d)
         (jassoc e f)
         (jassoc g h)
         (jassoc i j)
         (jassoc k l))))
  (^clojure.lang.Associative [^clojure.lang.Associative mp a b c d e f g h i j k l m n]
   (if (nil? mp)
     {a b c d e f g h i j k l m n}
     (-> (.assoc mp a b)
         (jassoc c d)
         (jassoc e f)
         (jassoc g h)
         (jassoc i j)
         (jassoc k l)
         (jassoc m n))))
  (^clojure.lang.Associative [^clojure.lang.Associative mp a b c d e f g h i j k l m n o p]
   (if (nil? mp)
     {a b c d e f g h i j k l m n o p}
     (-> (.assoc mp a b)
         (jassoc c d)
         (jassoc e f)
         (jassoc g h)
         (jassoc i j)
         (jassoc k l)
         (jassoc m n)
         (jassoc o p))))
  (^clojure.lang.Associative [^clojure.lang.Associative mp a b c d e f g h i j k l m n o p q r]
   (if (nil? mp)
     {a b c d e f g h i j k l m n o p q r}
     (-> (.assoc mp a b)
         (jassoc c d)
         (jassoc e f)
         (jassoc g h)
         (jassoc i j)
         (jassoc k l)
         (jassoc m n)
         (jassoc o p)
         (jassoc q r))))
  (^clojure.lang.Associative [^clojure.lang.Associative mp a b c d e f g h i j k l m n o p q r & pairs]
   (loop [r     (qassoc mp a b c d e f g h i j k l m n o p q r)
          pairs pairs]
     (if pairs
       (if (next pairs)
         (recur (jassoc r (first pairs) (second pairs)) (nnext pairs))
         (throw (IllegalArgumentException. "qassoc expects even number of arguments, found odd number")))
       r))))

(defn qupdate
  "Similar to `clojure.core/update`, updates a value in an associative structure,
  where `k` is a key and `f` is a function which will take the old value and any
  supplied args and return the new value, and returns a new structure. Uses `qassoc`
  instead of `clojure.core/assoc` internally.

  If the key does not exist, `nil` is passed as the old value."
  (^clojure.lang.Associative [^clojure.lang.Associative m k f]
   (qassoc m k (f (get m k))))
  (^clojure.lang.Associative [^clojure.lang.Associative m k f x]
   (qassoc m k (f (get m k) x)))
  (^clojure.lang.Associative[^clojure.lang.Associative m k f x y]
   (qassoc m k (f (get m k) x y)))
  (^clojure.lang.Associative [^clojure.lang.Associative m k f x y z]
   (qassoc m k (f (get m k) x y z)))
  (^clojure.lang.Associative [^clojure.lang.Associative m k f x y z c]
   (qassoc m k (f (get m k) x y z c)))
  (^clojure.lang.Associative [^clojure.lang.Associative m k f x y z c v]
   (qassoc m k (f (get m k) x y z c v)))
  (^clojure.lang.Associative [^clojure.lang.Associative m k f x y z c v a]
   (qassoc m k (f (get m k) x y z c v a)))
  (^clojure.lang.Associative [^clojure.lang.Associative m k f x y z c v a b]
   (qassoc m k (f (get m k) x y z c v a b)))
  (^clojure.lang.Associative [^clojure.lang.Associative m k f x y z c v a b & more]
   (qassoc m k (apply f (get m k) x y z c v a b more))))

(defn assoc-missing
  "Associates keys and values only if the keys do not yet exist in a map."
  ([]            nil)
  ([coll]       coll)
  ([coll k val] (if (contains? coll k) coll (qassoc coll k val)))
  ([coll k val & more]
   (if-not more
     (if (contains? coll k) coll (qassoc coll k val))
     (reduce (fn [acc [k v]] (if (contains? acc k) acc (qassoc acc k v)))
             (if (nil? coll) {} coll) (partition 2 (cons k (cons val more)))))))

(defn assoc-existing
  "Associates keys and values only if the keys exist in a map."
  ([]           nil)
  ([coll]       coll)
  ([coll k val] (if (contains? coll k) (qassoc coll k val) coll))
  ([coll k val & more]
   (if-not more
     (if (contains? coll k) (qassoc coll k val) coll)
     (reduce (fn [acc [k v]] (if (contains? acc k) (qassoc acc k v) acc))
             coll (partition 2 (cons k (cons val more)))))))

(defn update-existing
  "Updates the given key `k` of `coll` by calling a function `fun` and passing optional
  arguments specified as additional arguments. Will not perform any update if the
  given key does not exist within the collection. Returns updated collection.

  If `fun` is not a function it will be made one by using `constantly`."
  ([^clojure.lang.IPersistentMap coll k fun]
   (if (contains? coll k)
     (qassoc coll k (if (ifn? fun) (fun (get coll k)) fun))
     coll))
  ([^clojure.lang.IPersistentMap coll k fun & more]
   (if (contains? coll k)
     (qassoc coll k (if (ifn? fun) (apply fun (get coll k) more) fun))
     coll)))

(defn update-missing
  "Updates the given key `k` of `coll` by calling a function `fun` and passing optional
  arguments specified as additional arguments. Will not perform any update if the
  given key exists within the collection, therefore the function will always receive
  `nil` as its argument. Returns updated collection.

  If `fun` is not a function it will be made one by using `constantly`."
  ([coll k fun]
   (if (contains? coll k)
     coll
     (qassoc coll k (if (ifn? fun) (fun (get coll k)) fun))))
  ([coll k fun & more]
   (if (contains? coll k)
     coll
     (qassoc coll k (if (ifn? fun) (apply fun (get coll k) more) fun)))))

(defn update-if
  ([coll k pred fun]
   (if (contains? coll k)
     (let [v (get coll k)]
       (if (pred v)
         (qassoc coll k (fun v))
         coll))
     coll))
  ([coll k pred fun & more]
   (if (contains? coll k)
     (let [v (get coll k)]
       (if (pred v)
         (qassoc coll k (apply fun v more))
         coll))
     coll)))

(defn update-if-not
  ([coll k pred fun]
   (if (contains? coll k)
     (let [v (get coll k)]
       (if (pred v)
         coll
         (qassoc coll k (fun v))))
     coll))
  ([coll k pred fun & more]
   (if (contains? coll k)
     (let [v (get coll k)]
       (if (pred v)
         coll
         (qassoc coll k (apply fun v more))))
     coll)))

(defn update-to-bytes
  ([coll k]
   (update-if-not coll k bytes? normalize-to-bytes))
  ([coll k & keys]
   (reduce #(update-if-not %1 %2 bytes? normalize-to-bytes) coll (cons k keys))))

(defn update-bytes-to-strings
  ([coll k]
   (update-if coll k bytes? bytes-to-string))
  ([coll k & keys]
   (reduce #(update-if %1 %2 bytes? bytes-to-string) coll (cons k keys))))

(defmacro assoc-if
  ([coll pred k val]
   `(let [kol# ~coll]
      (if ~pred
        (qassoc kol# ~k ~val)
        kol#)))
  ([coll pred k val & pairs]
   `(let [kol# ~coll]
      (if ~pred
        (apply qassoc kol# ~k ~val ~@pairs)
        kol#))))

(defmacro assoc-if-not
  ([coll pred k val]
   `(let [kol# ~coll]
      (if ~pred
        kol#
        (qassoc kol# ~k ~val))))
  ([coll pred k val & pairs]
   `(let [kol# ~coll]
      (if ~pred
        kol#
        (apply qassoc kol# ~k ~val ~@pairs)))))

(defmacro assoc-if-key
  ([coll k pred val]
   `(let [kol# ~coll
          key# ~k]
      (if (~pred (get kol# key#))
        (qassoc kol# key# ~val)
        kol#))))

(defmacro assoc-if-not-key
  [coll k pred val]
  `(let [kol# ~coll
         key# ~k]
     (if (~pred (get kol# key#))
       kol#
       (qassoc kol# key# ~val))))

(defn dissoc-if
  [^clojure.lang.IPersistentMap m k
   ^clojure.lang.IFn pred]
  (if (pred (get m k)) (dissoc m k) m))

(defn remove-if-value
  ^clojure.lang.IPersistentMap
  [^clojure.lang.IPersistentMap m
   ^clojure.lang.IFn pred]
  (reduce-kv
   (fn [^clojure.lang.IPersistentMap mp k v]
     (if (pred v) (dissoc mp k) mp))
   m m))

(defn remove-if-value-in
  ^clojure.lang.IPersistentMap [^clojure.lang.IPersistentMap m vals]
  (if (nil? vals) m
      (if (< (count m) 1) m
          (let [vset (set vals)]
            (remove-if-value m #(contains? vset %))))))

(defn remove-if-value-not-in
  ^clojure.lang.IPersistentMap [^clojure.lang.IPersistentMap m vals]
  (if (nil? vals) (empty m)
      (if (< (count m) 1) m
          (let [vset          (set vals)
                not-contains? (complement contains?)]
            (remove-if-value m #(not-contains? vset %))))))

(defn remove-except
  ^clojure.lang.IPersistentMap [^clojure.lang.IPersistentMap m ^clojure.lang.ISeq keyseq]
  (select-keys m keyseq))

(defn remove-empty-values
  ^clojure.lang.IPersistentMap [^clojure.lang.IPersistentMap m]
  (remove-if-value
   m #(or (nil? %) (and (seqable? %) (nil? (seq %))))))

(defn map-vals-by-k
  "For each key and value of the given map m calls a function passed as the first
  argument (passing successive keys during calls to it) and generates a map with
  values updated by results returned by the function. When the third argument is
  given it should be a map on which operations are performed instead of using the
  original map. This may be helpful when we want to avoid merging the results with
  another map."
  {:added "1.0.0" :tag clojure.lang.IPersistentMap}
  ([^clojure.lang.IFn f
    ^clojure.lang.IPersistentMap m]
   (map-vals-by-k f m m))
  ([^clojure.lang.IFn f
    ^clojure.lang.IPersistentMap m
    ^clojure.lang.IPersistentMap dst]
   (if (nil? m)
     dst
     (reduce-kv
      (fn [^clojure.lang.IPersistentMap mp k v] (qassoc mp k (f k)))
      dst m))))

(defn map-vals-by-kv
  "For each key and value of the given map m calls a function passed as the first
  argument (passing successive keys and values during calls to it) and generates a
  map with values updated by results returned by the function. When the third
  argument is given it should be a map on which operations are performed instead of
  using the original map. This may be helpful when we want to avoid merging the
  results with another map."
  {:added "1.0.0" :tag clojure.lang.IPersistentMap}
  ([^clojure.lang.IFn f
    ^clojure.lang.IPersistentMap m]
   (map-vals-by-kv f m m))
  ([^clojure.lang.IFn f
    ^clojure.lang.IPersistentMap m
    ^clojure.lang.IPersistentMap dst]
   (if (nil? m)
     dst
     (reduce-kv
      (fn [^clojure.lang.IPersistentMap mp k v] (qassoc mp k (f k v)))
      dst m))))

(defn map-vals
  "For each key and value of the given map m calls a function passed as the first
  argument (passing successive values during calls to it) and generates a map with
  values updated by results returned by the function. When the third argument is
  given it should be a map on which operations are performed instead of using the
  original map. This may be helpful when we want to avoid merging the results with
  another map."
  {:added "1.0.0" :tag clojure.lang.IPersistentMap}
  ([^clojure.lang.IFn f
    ^clojure.lang.IPersistentMap m]
   (map-vals f m m))
  ([^clojure.lang.IFn f
    ^clojure.lang.IPersistentMap m
    ^clojure.lang.IPersistentMap dst]
   (if (nil? m)
     dst
     (reduce-kv
      (fn [^clojure.lang.IPersistentMap mp k v] (qassoc mp k (f v)))
      dst m))))

(defn map-keys-by-v
  "For each key and value of the given map m calls a function passed as the first
  argument (passing successive values during calls to it) and generates a map with
  keys updated by results returned by the function. When the third argument is
  given then it should be a map on which operations are performed instead of using
  and empty map."
  {:added "1.0.0" :tag clojure.lang.IPersistentMap}
  ([^clojure.lang.IFn f
    ^clojure.lang.IPersistentMap m]
   (map-keys-by-v f m {}))
  ([^clojure.lang.IFn f
    ^clojure.lang.IPersistentMap m
    ^clojure.lang.IPersistentMap dst]
   (if (nil? m)
     dst
     (reduce-kv
      (fn [^clojure.lang.IPersistentMap mp k v] (qassoc mp (f v) v))
      dst m))))

(defn map-keys
  "For each key and value of the given map m calls a function passed as the first
  argument (passing successive keys during calls to it) and generates a map with keys
  updated by results returned by the function. When the third argument is given then
  it should be a map on which operations are performed instead of using an empty
  map."
  {:added "1.0.0" :tag clojure.lang.IPersistentMap}
  ([^clojure.lang.IFn f
    ^clojure.lang.IPersistentMap m]
   (map-keys f m {}))
  ([^clojure.lang.IFn f
    ^clojure.lang.IPersistentMap m
    ^clojure.lang.IPersistentMap dst]
   (if (nil? m)
     dst
     (reduce-kv
      (fn [^clojure.lang.IPersistentMap mp k v] (qassoc mp (f k) v))
      dst m))))

(defn map-keys-and-vals
  "For each key and value of the given map m calls a function passed as the first
  argument (passing successive keys during calls to it) and generates a map with keys
  updated by results returned by the function and values also updated by results of
  the same function. The function should return a sequential collection of 2
  elements: first containing a new value of a key and second containing a new value
  of a transformed value associated with that key. When the third argument is given
  then it should be a map on which operations are performed instead of using an empty
  map."
  {:added "1.0.0" :tag clojure.lang.IPersistentMap}
  ([^clojure.lang.IFn f
    ^clojure.lang.IPersistentMap m]
   (map-keys-and-vals f m {}))
  ([^clojure.lang.IFn f
    ^clojure.lang.IPersistentMap m
    ^clojure.lang.IPersistentMap dst]
   (if (nil? m)
     dst
     (reduce-kv
      (fn [^clojure.lang.IPersistentMap mp k v]
        (let [[new-k new-v] (f k v)] (qassoc mp new-k new-v)))
      dst m))))

(defn map-of-sets-invert
  "Like `clojure.set/map-invert` but for map of sets (as values) to preserve all
  possible values (as keys of newly created map)."
  {:added "1.0.0" :tag clojure.lang.IPersistentMap}
  [^clojure.lang.IPersistentMap m]
  (if (nil? m)
    nil
    (reduce (fn [^clojure.lang.IPersistentMap am [k v]]
              (qassoc am k (conj (am k (hash-set)) v)))
            {}
            (for [[k st] m v st] [v k]))))

(defn invert-in-sets
  "Like `clojure.set/map-invert` but preserves all possible values in sets."
  {:added "1.0.0" :tag clojure.lang.IPersistentMap}
  ([^clojure.lang.IPersistentMap m]
   (invert-in-sets m #{}))
  ([^clojure.lang.IPersistentMap m ^clojure.lang.PersistentHashSet dst]
   (persistent!
    (reduce (fn [am [k v]]
              (assoc! am v (conj (am v dst) k)))
            (transient {}) m))))

(defn map-of-vectors-invert-flatten
  "Like `clojure.set/map-invert` but for map of vectors (as values). Duplicated keys
  are replaced."
  {:added "1.0.0" :tag clojure.lang.IPersistentMap}
  [^clojure.lang.IPersistentMap m]
  (->> (mapcat (fn [[k v]] (interleave v (repeat k))) m)
       (partition 2)
       (map vec)
       (into {})))

(defn map-values
  "Recursively transforms values of a map coll using function f. The function should
  take a value and return new value."
  [^clojure.lang.IFn f, ^clojure.lang.IPersistentMap coll]
  (if (some? coll)
    (reduce-kv (fn [^clojure.lang.IPersistentMap m, k, v]
                 (qassoc m k (if (map? v) (map-values f v) (f v))))
               (empty coll) coll)))

(defn- map-values-with-path-core
  "Recursively transforms values of a map coll using function f. The function should
  take a value and a sequence of visited keys in reverse order, and return a new
  value. Third argument should be an initial key-path."
  ([^clojure.lang.IFn f, ^clojure.lang.IPersistentMap coll, kpath]
   (if (some? coll)
     (reduce-kv (fn [m k v]
                  (qassoc m k (if (map? v)
                                    (map-values-with-path-core f v (conj kpath k))
                                    (f v (conj kpath k)))))
                (empty coll) coll))))

(defn map-values-with-rpath
  "Recursively transforms values of a map coll using function f. The function should
  take a value and a sequence of visited keys in reverse order (stored in a
  persistent list), and return a new value."
  [^clojure.lang.IFn f, ^clojure.lang.IPersistentMap coll]
  (map-values-with-path-core f coll ()))

(defn map-values-with-path
  "Recursively transforms values of a map coll using function f. The function should
  take a value and a sequence of visited keys (stored in a vector), and return a new
  value."
  [^clojure.lang.IFn f, ^clojure.lang.IPersistentMap coll]
  (map-values-with-path-core f coll []))

(defn update-values
  "Returns the map with its values identified with keys from vmap updated with the
  associated functions from vmap."
  {:added "1.0.0" :tag clojure.lang.IPersistentMap}
  ([^clojure.lang.IPersistentMap map
    ^clojure.lang.IPersistentMap vmap]
   (update-values map vmap false))
  ([^clojure.lang.IPersistentMap map
    ^clojure.lang.IPersistentMap vmap
    ^Boolean create-keys?]
   (if (some? map)
     (if create-keys?
       (reduce-kv
        (fn [^clojure.lang.IPersistentMap mp k v]
          (qassoc mp k (if (fn? v) (v (get mp k)) v)))
        map vmap)
       (reduce-kv
        (fn [^clojure.lang.IPersistentMap mp k v]
          (if (contains? mp k)
            (qassoc mp k (if (fn? v) (v (get mp k)) v))
            mp))
        map vmap))))
  ([^clojure.lang.IPersistentMap map
    ^clojure.lang.IPersistentMap vmap
    ^Boolean create-keys?
    ^clojure.lang.Keyword remove-key-mark]
   (if (some? map)
     (if create-keys?
       (reduce-kv
        (fn [^clojure.lang.IPersistentMap mp k v]
          (let [r (if (fn? v) (v (get mp k)) v)]
            (if (identical? r remove-key-mark)
              (dissoc mp k)
              (qassoc mp k r))))
        map vmap)
       (reduce-kv
        (fn [^clojure.lang.IPersistentMap mp k v]
          (if (contains? mp k)
            (let [r (if (fn? v) (v (get mp k)) v)]
              (if (identical? r remove-key-mark)
                (dissoc mp k)
                (qassoc mp k r)))
            mp))
        map vmap)))))

(defn update-values-recur
  "Returns the map with its values identified with keys from vmap recursively updated
  with the associated functions from vmap. Shape is not reflected, second map (vmap)
  should be flat, searching for keys is recursive, including nested vectors."
  {:added "1.0.0" :tag clojure.lang.IPersistentMap}
  ([^clojure.lang.IPersistentMap map
    ^clojure.lang.IPersistentMap vmap]
   (update-values-recur map vmap false))
  ([^clojure.lang.IPersistentMap map
    ^clojure.lang.IPersistentMap vmap
    ^Boolean create-keys?]
   (if (some? map)
     (reduce-kv
      (fn [^clojure.lang.IPersistentMap mp k v]
        (if (or (map? v) (vector? v))
          (qassoc mp k (update-values-recur v vmap create-keys?))
          mp))
      (if (vector? map)
        map
        (update-values map vmap create-keys?)) map))))

(defn- update-values-redux
  [^clojure.lang.IPersistentMap mp k v]
  (if (contains? mp k)
    (let [funik (if (fn? v) v (constantly v))]
      (qassoc mp k (let [val (get mp k)]
                     (if (vector? val)
                       (vec (clojure.core/map funik val))
                       (if (sequential? val)
                         (clojure.core/map funik val)
                         (funik val))))))
    mp))

(defn update-values-or-seqs
  "Returns the map with its values identified with keys from `vmap` updated with the
  associated functions from `vmap`."
  {:added "1.0.0" :tag clojure.lang.IPersistentMap}
  ([^clojure.lang.IPersistentMap map
    ^clojure.lang.IPersistentMap vmap]
   (update-values-or-seqs map vmap false))
  ([^clojure.lang.IPersistentMap map
    ^clojure.lang.IPersistentMap vmap
    ^Boolean create-keys?]
   (if (some? map)
     (if create-keys?
       (reduce-kv
        (fn [^clojure.lang.IPersistentMap mp k v]
          (qassoc mp k (if (fn? v) (v (get mp k)) v)))
        map vmap)
       (reduce-kv
        update-values-redux
        map vmap)))))

(defn update-values-or-seqs-recur
  "Returns the map with its values identified with keys from `vmap` recursively updated
  with the associated functions from `vmap`. Shape is not reflected, second
  map (`vmap`) should be flat, searching for keys is recursive, including nested
  vectors."
  {:added "1.0.0" :tag clojure.lang.IPersistentMap}
  ([^clojure.lang.IPersistentMap map
    ^clojure.lang.IPersistentMap vmap]
   (update-values-or-seqs-recur map vmap false))
  ([^clojure.lang.IPersistentMap map
    ^clojure.lang.IPersistentMap vmap
    ^Boolean create-keys?]
   (if (some? map)
     (reduce-kv
      (fn [^clojure.lang.IPersistentMap mp k v]
        (if (or (map? v) (vector? v))
          (if (and (contains? mp k) (contains? vmap k))
            (let [mp (update-values-redux mp k (get vmap k))
                  v  (get mp k)]
              (if (or (map? v) (vector? v))
                (qassoc mp k (update-values-or-seqs-recur v vmap create-keys?))
                mp))
            (qassoc mp k (update-values-or-seqs-recur v vmap create-keys?)))
          mp))
      (if (vector? map)
        map
        (update-values-or-seqs map vmap create-keys?))
      map))))

(defn dissoc-in
  "Like `clojure.core/assoc-in` but removes entries. Leaves empty maps."
  {:added "1.0.0" :tag clojure.lang.IPersistentMap}
  [^clojure.lang.IPersistentMap m [k & ks :as keys]]
  (if ks
    (if-some [nmap (get m k)]
      (qassoc m k (dissoc-in nmap ks))
      m)
    (dissoc m k)))

(defn duplicate-keys
  "Returns a map `map` with the keys present in `kmap` duplicated under new names
  according to the values in `kmap`."
  [map kmap]
  (if (nil? kmap)
    map
    (reduce
     (fn [m [old new]]
       (if (contains? map old)
         (qassoc m new (get map old))
         m))
     map kmap)))

(defn nil-keys
  [m keys]
  (if (some? m)
    (if-some [keys (seq keys)]
      (apply qassoc m (interleave keys (repeat nil)))
      m)))

(defn nil-existing-keys
  [m keys]
  (if (some? m)
    (if-some [keys (seq (filter (partial contains? m) keys))]
      (apply qassoc m (interleave keys (repeat nil)))
      m)))

(defmacro lazy
  "Creates a lazy map from a literal map. All values are unrealized."
  ([]
   `(lazy-map/lazy-map nil))
  ([m]
   (#'lazy-map/lazy-map &form &env m)))

(defn to-lazy
  "Converts the given map to a lazy map."
  ([]
   (lazy))
  ([m]
   (lazy-map/->?LazyMap m)))

(defn lazy?
  "Returns `true` if the given argument is a lazy map."
  [m]
  (instance? LazyMap m))

(defn select-keys-lazy
  "Like `clojure.core/select-keys` but preserves unrealized values as they are."
  [m keyseq]
  (loop [ret  (lazy-map/lazy-map nil)
         keys (seq keyseq)]
    (if keys
      (let [entry (find m (first keys))]
        (recur
         (if entry
           (qassoc ret (.key_ ^LazyMapEntry entry) (.val_ ^LazyMapEntry entry))
           ret)
         (next keys)))
      ret)))

(defn merge-lazy
  "Merges two lazy maps."
  [m1 m2]
  (let [c1 (count m1)
        c2 (count m2)]
    (if (pos? c1)
      (if (pos? c2)
        (if (> c1 c2)
          (reduce (if (lazy? m2)
                    #(qassoc %1 (.key_ ^LazyMapEntry %2) (.val_ ^LazyMapEntry %2))
                    conj)
                  (lazy-map/->?LazyMap m1) m2)
          (reduce (if (lazy? m1)
                    #(let [k (.key_ ^LazyMapEntry %2)]
                       (if (contains? %1 k) %1 (qassoc %1 k (.val_ ^LazyMapEntry %2))))
                    #(if (contains? %1 (key %2)) %1 (conj %1 %2)))
                  (lazy-map/->?LazyMap m2) m1))
        (lazy-map/->?LazyMap m1))
      (if m2 (lazy-map/->?LazyMap m2)))))
