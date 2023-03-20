(ns

    ^{:doc    "Random utils, database helper functions."
      :author "Paweł Wilk"
      :added  "1.0.0"}

    io.randomseed.utils.db

  (:refer-clojure :exclude [memoize parse-long uuid random-uuid])

  (:require [clojure.string                :as            str]
            [clojure.core.memoize          :as            mem]
            [clojure.core.cache            :as          cache]
            [clojure.core.cache.wrapped    :as            cwr]
            [camel-snake-kebab.core        :as            csk]
            [next.jdbc                     :as           jdbc]
            [next.jdbc.sql                 :as            sql]
            [next.jdbc.result-set          :as             rs]
            [next.jdbc.sql.builder         :as        builder]
            [taoensso.nippy                :as          nippy]
            [io.randomseed.utils           :refer        :all]
            [io.randomseed.utils.log       :as            log]
            [io.randomseed.utils.var       :as            var]
            [io.randomseed.utils.map       :as            map]
            [io.randomseed.utils.map       :refer    [qassoc]]
            [io.randomseed.utils.time      :as           time]
            [io.randomseed.utils.nop-cache :as      nop-cache])

  (:import (javax.sql DataSource)))

(def ^:const underscore (re-pattern "_"))
(def ^:const dash       (re-pattern "-"))

;; Common identifier conversions

(defn id-from-db
  "Converts the given ID retrieved from a database to a value suitable to be used in
  Clojure programs. If `v` is a number or a keyword, it is returned as is. Otherwise
  it is converted to a keyword."
  [v]
  (if v (if (or (number? v) (keyword? v)) v (keyword v))))

(defn id-to-db
  "Converts the given ID to a value suitable to be stored in a database. If `v` is a
  number, it is passed as is. Otherwise it is converted to a string."
  [v]
  (if v (if (number? v) v (some-str v))))

(defn id-as-str
  "Tries to convert the given argument to a string identifier."
  [v]
  (if v
    (if (string? v) v (str (if (ident? v) (symbol v) v)))))

;; Caching (more precise and granulate control over caching than memoization)

(defn cache-prepare
  "Prepares a cache object of the given TTL and/or queue size. Optionally it can get an
  initial map of entries. Returns a cache object."
  ([ttl]
   (cache-prepare ttl nil nil))
  ([ttl queue-size]
   (cache-prepare ttl queue-size nil))
  ([ttl queue-size initial-map]
   (let [ttl         (if ttl (time/millis ttl))
         ttl         (if (pos-int? ttl) ttl)
         qsize       (if (pos-int? queue-size) queue-size)
         initial-map (or initial-map {})
         c           initial-map
         c           (if qsize (cache/fifo-cache-factory c :threshold qsize) c)
         c           (if ttl   (cache/ttl-cache-factory  c :ttl ttl) c)]
     (if (identical? c initial-map)
       (nop-cache/factory)
       c))))

(defn cache-create
  "Creates a cache object of the given TTL and/or queue size. Optionally it can get an
  initial map of entries. Returns cache object encapsulated in an atom."
  ([ttl]
   (atom (cache-prepare ttl nil nil)))
  ([ttl queue-size]
   (atom (cache-prepare ttl queue-size nil)))
  ([ttl queue-size initial-map]
   (atom (cache-prepare ttl queue-size initial-map))))

(defn cache-evict!
  "Removes entry or entries from the cache. Returns the updated cache from the atom."
  ([cache-atom entry]
   (cwr/evict cache-atom entry))
  ([cache-atom entry & more]
   (swap! cache-atom (partial reduce cache/evict) (cons entry more))))

(def not-found ::not-found)

(defn cwr-lookup
  "Performs a cache lookup of `id` on `cache` and returns the hit. If there is no
  element found, returns the keyword `:io.randomseed.utils.db/not-found`."
  [cache id]
  (cwr/lookup cache id ::not-found))

(defn not-found?
  "Returns `true` when the given value equals to `:io.randomseed.utils.db/not-found`."
  [e]
  (identical? ::not-found e))

(defn cache-lookup-coll
  "Looks for a collection of entries identified by the given ID in a cache which should
  be a cache object encapsulated in an atom. Returns a map with identifiers as keys
  and values for all found entries. Entries which are missing in the cache are
  grouped under the `:io.randomseed.utils.db/not-found` key as a list."
  [cache ids]
  (if (seq ids)
    (let [ids (map id-from-db ids)]
      (reduce (fn [m id]
                (let [props (cwr-lookup cache id)]
                  (if (not-found? props)
                    (qassoc m ::not-found (conj (get m ::not-found) id))
                    (qassoc m id props))))
              {} ids))))

(defn cache-lookup
  "Looks for the entry of the given ID in a cache which should be a cache object
  encapsulated in an atom. For multiple IDs, calls `cache-lookup-coll`. If the entry
  was not found, returns `:io.randomseed.utils.db/not-found`."
  ([cache id]
   (cwr-lookup cache (id-from-db id)))
  ([cache id & ids]
   (cache-lookup-coll cache (cons id ids))))

;; Memoization

(defn memoize
  "Creates memoized version of a database accessing or other function. With only 1
  argument defaults to a FIFO cache with length of 256 and TTL cache with expiration
  of 150 seconds. When 2 arguments are given it only creates FIFO cache of the given
  length, without TTL. When `queue-size` is `nil` or <= 0, the FIFO cache will not be
  created. When `ttl` is `nil` or <= 0, the TTL cache will not be created."
  ([f]
   (memoize f 256 150000))
  ([f queue-size]
   (memoize f queue-size nil))
  ([f queue-size ttl]
   (mem/memoizer f (cache-prepare ttl queue-size {}))))

(defn memoizer
  "Creates a memoized functions with predefined TTL and queue size taken from
  config. If the function is not given it will try to dereference symbol present in
  the config under the `:memoizer` key. Uses `io.randomseed.utils.db/memoize` to
  initialize caches."
  ([config]
   (if-some [f (var/deref-symbol (:memoizer config))]
     (memoizer f config)))
  ([f config]
   (let [cache-size (:cache-size config)
         cache-ttl  (:cache-ttl  config)
         cache-ttl  (if cache-ttl (time/millis cache-ttl))]
     (if (or (pos-int? cache-size) (pos-int? cache-ttl))
       (memoize f cache-size cache-ttl)
       f))))

(defn invalidate!
  "Invalidates cache associated with memoized function `f`. If `key-params` are given
  it should be a cache key sequence or other structure (usually function
  arguments). If only function is given, it clears the whole cache."
  ([f]            (mem/memo-clear! f))
  ([f key-params] (mem/memo-clear! f key-params)))

(defn cache-id+
  "Returns metadata value associated with `:io.randomseed.utils.db/cache` key for the
  function `f`."
  [f]
  (::cache (meta f)))

(defn invalidate+!
  "Invalidates cache associated with memoized function `f`. If `key-params` are given
  it should be a cache key sequence or other structure (usually function arguments),
  internally transformed to vector. If only function is given, it clears the whole
  cache. Supports functions memoized with `memoize+`."
  ([f]
   (if-some [cache (cache-id+ f)] (reset! cache {}))
   (invalidate! f))
  ([f key-params]
   (if-some [cache (cache-id+ f)]
     (let [kp (if (vector? key-params) key-params (vec key-params))
           kp (if (> (count kp) 8) (conj (subvec kp 0 8) (seq (subvec kp 8))) kp)]
       (if (contains? @cache kp) (swap! cache dissoc kp))))
   (invalidate! f key-params)))

(defn memoized+?
  "Returns `true` if the given function `f` is memoized using `memoize+`. If not,
  returns `false`."
  [f]
  (boolean (and f (cache-id+ f))))

(defn invalidator
  "Generates invalidation function for the given memoized function `f`. The
  invalidation function takes arguments in the same way as memoized function. If they
  match a value in the associated cache, the entry is evicted. Supports functions
  memoized with `memoize` and `memoize+`."
  [f]
  (if (and f (mem/memoized? f))
    (if (memoized+? f)
      (fn [& key-params] (invalidate+! f key-params))
      (fn [& key-params] (invalidate!  f key-params)))
    (constantly nil)))

(defn memoize+
  "Wrapper around `clojure.core.memoize/fifo` which uses fast, map-based memoization
  for cache size up to `level1-size` size, and then switches to slower memoization
  based on FIFO cache of `level2-size` size. The first level of cache is never purged
  automatically; once an element is there, it stays.

  If only one size is given (`size`), the level-1 cache size will be set to 70% of
  it, and level-2 cache to 30% of it.

  If no size is given, the level-1 cache will have a size of 89000 elements and
  level-2 cache will have a size of 39000 elements; both having a size of 128000
  elements in total.

  Be aware that caching key in level-1 cache is a vector of all arguments unless the
  argument count is greater than 8. In such case all extra arguments (expressed with
  native Clojure structure used for variadic function arguments) are grouped as 9th
  element of this vector.

  To control caches associated with original function, use a metadata of the returned
  function object."
  ([f]
   (memoize+ f 128000))
  ([f size]
   (let [level1-size (int (/ (* size 7) 10))
         level2-size (- size level1-size)]
     (memoize+ f level1-size level2-size)))
  ([f level1-size level2-size]
   (let [level1-size (unchecked-int level1-size)
         level2-size (long level2-size)
         L1-cache    (atom {})
         L2-fn       (mem/fifo f {} :fifo/threshold level2-size)]
     (-> (fn
           ([]
            (if-let [e (find @L1-cache nil)]
              (val e)
              (if (< (count @L1-cache) level1-size)
                (let [r (f)] (swap! L1-cache qassoc nil r) r)
                (L2-fn))))
           ([a]
            (let [^clojure.lang.PersistentVector args [a]]
              (if-let [e (find @L1-cache args)]
                (val e)
                (if (< (count @L1-cache) level1-size)
                  (let [r (f a)] (swap! L1-cache qassoc args r) r)
                  (L2-fn a)))))
           ([a b]
            (let [^clojure.lang.PersistentVector args [a b]]
              (if-let [e (find @L1-cache args)]
                (val e)
                (if (< (count @L1-cache) level1-size)
                  (let [r (f a b)] (swap! L1-cache qassoc args r) r)
                  (L2-fn a b)))))
           ([a b c]
            (let [^clojure.lang.PersistentVector args [a b c]]
              (if-let [e (find @L1-cache args)]
                (val e)
                (if (< (count @L1-cache) level1-size)
                  (let [r (f a b c)] (swap! L1-cache qassoc args r) r)
                  (L2-fn a b c)))))
           ([a b c d]
            (let [^clojure.lang.PersistentVector args [a b c d]]
              (if-let [e (find @L1-cache args)]
                (val e)
                (if (< (count @L1-cache) level1-size)
                  (let [r (f a b c d)] (swap! L1-cache qassoc args r) r)
                  (L2-fn a b c d)))))
           ([a b c d e]
            (let [^clojure.lang.PersistentVector args [a b c d e]]
              (if-let [e (find @L1-cache args)]
                (val e)
                (if (< (count @L1-cache) level1-size)
                  (let [r (f a b c d e)] (swap! L1-cache qassoc args r) r)
                  (L2-fn a b c d e)))))
           ([a b c d e x]
            (let [^clojure.lang.PersistentVector args [a b c d e x]]
              (if-let [e (find @L1-cache args)]
                (val e)
                (if (< (count @L1-cache) level1-size)
                  (let [r (f a b c d e x)] (swap! L1-cache qassoc args r) r)
                  (L2-fn a b c d e x)))))
           ([a b c d e x g]
            (let [^clojure.lang.PersistentVector args [a b c d e x g]]
              (if-let [e (find @L1-cache args)]
                (val e)
                (if (< (count @L1-cache) level1-size)
                  (let [r (f a b c d e x g)] (swap! L1-cache qassoc args r) r)
                  (L2-fn a b c d e x g)))))
           ([a b c d e x g h]
            (let [^clojure.lang.PersistentVector args [a b c d e x g h]]
              (if-let [e (find @L1-cache args)]
                (val e)
                (if (< (count @L1-cache) level1-size)
                  (let [r (f a b c d e x g h)] (swap! L1-cache qassoc args r) r)
                  (L2-fn a b c d e x g h)))))
           ([a b c d e x g h & more]
            (let [^clojure.lang.PersistentVector k [a b c d e x g h more]]
              (if-let [e (find @L1-cache k)]
                (val e)
                (if (< (count @L1-cache) level1-size)
                  (let [r (apply f a b c d e x g h more)] (swap! L1-cache qassoc k r) r)
                  (apply L2-fn a b c d e x g h more))))))
         (with-meta (qassoc (or (meta L2-fn) {}) ::cache L1-cache ::original f))))))

;; Builder and conversion functions

(def to-lisp          (memoize+ to-lisp-str         1024 512))
(def to-lisp-simple   (memoize+ to-lisp-simple-str   512 128))
(def to-lisp-slashed  (memoize+ to-lisp-slashed-str  512 128))
(def to-snake         (memoize+ to-snake-str        1024 512))
(def to-snake-simple  (memoize+ to-snake-simple-str  512 128))
(def to-snake-slashed (memoize+ to-snake-slashed-str 512 128))

(def opts-map
  {:return-keys  false
   :builder-fn   rs/as-modified-maps
   :column-fn    to-snake
   :table-fn     to-snake
   :qualifier-fn to-lisp
   :label-fn     to-lisp})

(def opts-simple-map
  {:return-keys  false
   :builder-fn   rs/as-unqualified-modified-maps
   :column-fn    to-snake
   :table-fn     to-snake
   :qualifier-fn to-lisp
   :label-fn     to-lisp})

(def opts-slashed-map
  {:return-keys  false
   :builder-fn   rs/as-unqualified-modified-maps
   :column-fn    to-snake
   :table-fn     to-snake
   :qualifier-fn (constantly nil)
   :label-fn     to-lisp-slashed})

(def opts-vec
  {:return-keys  false
   :builder-fn   rs/as-modified-arrays
   :column-fn    to-snake
   :table-fn     to-snake
   :qualifier-fn to-lisp
   :label-fn     to-lisp})

(def opts-simple-vec
  {:return-keys  false
   :builder-fn   rs/as-unqualified-modified-arrays
   :column-fn    to-snake
   :table-fn     to-snake
   :qualifier-fn to-lisp
   :label-fn     to-lisp})

(def opts-slashed-vec
  {:return-keys  false
   :builder-fn   rs/as-unqualified-modified-arrays
   :column-fn    to-snake
   :table-fn     to-snake
   :qualifier-fn (constantly nil)
   :label-fn     to-lisp-slashed})

(defn join-col-names
  [cols]
  (str/join "," (map to-snake-simple cols)))

(defn braced-join-col-names
  [cols]
  (str "(" (str/join "," (map to-snake-simple cols)) ")"))

(defn braced-join-col-names-no-conv
  [cols]
  (str "(" (str/join "," cols) ")"))

(defn join-?
  [ids]
  (str/join "," (map (constantly "?") ids)))

(defn braced-join-?
  [ids]
  (str "(" (str/join "," (map (constantly "?") ids)) ")"))

(defn join-v=?
  [ids]
  (str/join ", " (map #(str % " = ?") ids)))

(defn values-?
  [coll]
  (str "VALUES (" (str/join "," (map (constantly "?") coll)) ")"))

(defn braced-?
  [coll]
  (str "(" (str/join "," (map (constantly "?") coll)) ")"))

(defn- mk-keyword
  ([n]
   (mk-keyword n nil))
  ([ns n]
   (if (nil? n)
     (if (keyword? ns) ns (keyword (if (string? ns) ns (str ns))))
     (if ns
       (keyword
        (if (string? ns) ns (if (ident? ns) (name ns) (str ns)))
        (if (string?  n)  n (if (ident?  n) (name  n) (str  n))))))))

;; Type checks

(defn data-source?
  [v]
  (instance? DataSource v))

;; Getter and setter generators

(defn make-getter-coll
  "Creates a database getter suitable for use with `get-cached-coll-` family of
  functions. The returned function should accept an argument containing multiple
  identifiers."
  ([id-col]
   (make-getter-coll nil id-col nil))
  ([id-col cols]
   (make-getter-coll nil id-col cols))
  ([table id-col cols]
   (let [id-col (keyword id-col)
         cols   (if (map? cols) (keys cols) cols)
         cols   (if (coll? cols) (seq cols) cols)
         cols   (if (coll? cols) cols [(or cols "*")])
         table  (to-snake-simple table)
         q      (str-spc "SELECT" (join-col-names cols)
                         "FROM"   (or table "?")
                         "WHERE"  (to-snake-simple id-col)
                         "IN (")]
     (if table
       (fn db-getter-coll
         ([db ids]
          (db-getter-coll db nil ids))
         ([db _ ids]
          (if-some [ids (seq ids)]
            (let [ids   (map id-to-db ids)
                  query (str q (join-? ids) ")")]
              (->> (sql/query db (cons query ids) opts-simple-map)
                   (reduce #(qassoc %1 (id-from-db (get %2 id-col)) %2) {}))))))
       (fn [db table ids]
         (if-some [ids (seq ids)]
           (let [ids   (map id-to-db ids)
                 table (to-snake-simple table)
                 query (str q (join-? ids) ")")]
             (->> (sql/query db (cons query (cons table ids)) opts-simple-map)
                  (reduce #(qassoc %1 (id-from-db (get %2 id-col)) %2) {})))))))))

(defn make-getter
  ([id-col cols]
   (make-getter nil id-col cols nil))
  ([table id-col cols]
   (make-getter table id-col cols nil))
  ([table id-col cols getter-coll-fn]
   (let [id-col (keyword id-col)
         cols   (if (map? cols)  (keys cols) cols)
         cols   (if (coll? cols) (seq cols) cols)
         cols   (if (coll? cols) cols [(or cols "*")])
         table  (to-snake-simple table)
         q      (str-spc "SELECT" (join-col-names cols)
                         "FROM"   (or table "?")
                         "WHERE"  (to-snake-simple id-col) "= ?")]
     (if table
       (if getter-coll-fn
         (fn db-getter
           ([db id]   (db-getter db nil id))
           ([db _ id] (jdbc/execute-one! db [q (id-to-db id)] opts-simple-map))
           ([db _ id & more] (getter-coll-fn db (cons id more))))
         (fn [db _ id]
           (jdbc/execute-one! db [q (id-to-db id)] opts-simple-map)))
       (if getter-coll-fn
         (fn
           ([db table id]
            (jdbc/execute-one! db [q (to-snake-simple table) (id-to-db id)]
                               opts-simple-map))
           ([db table id & more]
            (getter-coll-fn db table (cons id more))))
         (fn [db table id]
           (jdbc/execute-one! db [q (to-snake-simple table) (id-to-db id)]
                              opts-simple-map)))))))

(defn make-setter
  ([id-col]
   (make-setter nil id-col))
  ([table id-col]
   (let [id-col (to-snake-simple id-col)
         table  (to-snake-simple table)]
     (if table
       (fn db-setter
         ([db _ id kvs] (sql/update! db table kvs {id-col id} opts-simple-map))
         ([db id kvs]   (sql/update! db table kvs {id-col id} opts-simple-map)))
       (fn db-setter-table
         ([db table id kvs] (sql/update! db table kvs {id-col id} opts-simple-map)))))))

(defn make-deleter
  ([id-col]
   (make-deleter nil id-col))
  ([table id-col]
   (let [id-col (keyword id-col)
         table  (to-snake-simple table)
         q      (str-spc "DELETE FROM" (or table "?")
                         "WHERE" (to-snake-simple id-col) "= ?")]
     (if table
       (fn db-deleter
         ([db _ id]
          (db-deleter db id))
         ([db id]
          (jdbc/execute-one! db [q (id-to-db id)] opts-simple-map)))
       (fn db-deleter-table
         ([db table id]
          (jdbc/execute-one! db [q (to-snake-simple table) (id-to-db id)]
                             opts-simple-map)))))))

;; Generic getters

(defn get-ids
  "Gets a map of ID-to-properties from a database for the given IDs and a
  table. Assumes each result will be related to a single, unique ID."
  [db table ids]
  (if (seq ids)
    (let [ids (map id-to-db ids)]
      (->> opts-simple-map
           (sql/find-by-keys db table (cons (str "id IN " (braced-join-? ids)) ids))
           (reduce #(qassoc %1 (id-from-db (get %2 :id)) %2) {})))))

(defn get-id
  "Gets properties of the given ID from a database table. For multiple IDs, calls
  get-ids."
  ([db table id]
   (sql/get-by-id db table (id-to-db id) opts-simple-map))
  ([db table id & more]
   (apply get-ids db table (cons id more))))

;; Cached getters and setters

(defn get-cached-coll
  "Returns a (possibly cached) sequence of maps requested using db-getter for the given
  IDs. When called with a table name it will be passed as a second argument to the
  given getter function. When no getter is given the standard one (get-ids) is
  used. Use make-getter-coll to create getter (with or without predefined table
  name)."
  {:arglists '([cache db-getter db ids]
               [cache table     db ids]
               [cache table     db-getter db ids])}
  ([cache db-getter-or-table db ids]
   (if (fn? db-getter-or-table)
     (get-cached-coll cache nil db-getter-or-table db ids)
     (get-cached-coll cache db-getter-or-table get-ids db ids)))
  ([cache table db-getter db ids]
   (let [looked-up (cache-lookup-coll cache ids)
         not-found (seq (get looked-up ::not-found))]
     (if-not not-found
       looked-up
       (let [from-db (db-getter db table not-found)]
         (reduce #(qassoc %1 %2 (cwr/lookup-or-miss cache %2 from-db))
                 (dissoc looked-up ::not-found)
                 not-found))))))

(defn get-cached
  "Returns a (possibly cached) sequence of maps requested using db-getter for the given
  IDs. A database connection and a table name can be passed to be used with the
  standard get-id getter function instead. When multiple IDs are given, calls
  get-cached-coll."
  {:arglists '([cache db-getter db id]
               [cache db-getter db id & more]
               [cache table db id]
               [cache table db id & more]
               [cache table db-getter db id]
               [cache table db-getter db id & more])}
  ([cache db-getter-or-table db id]
   (if (fn? db-getter-or-table)
     (cwr/lookup-or-miss cache (id-from-db id) #(db-getter-or-table db nil %))
     (cwr/lookup-or-miss cache (id-from-db id) #(get-id db db-getter-or-table %))))
  ([cache db-getter-or-table db-or-getter id-or-db id2-or-id]
   (if (data-source? db-or-getter)
     (get-cached-coll cache nil db-getter-or-table db-or-getter [id-or-db id2-or-id])
     (cwr/lookup-or-miss cache (id-from-db id2-or-id) #(db-or-getter id-or-db db-getter-or-table %))))
  ([cache db-getter-or-table db-or-getter id-or-db id2-or-id & more]
   (if (data-source? db-or-getter)
     (get-cached-coll cache db-getter-or-table db-or-getter (cons id-or-db (cons id2-or-id more)))
     (get-cached-coll cache db-getter-or-table db-or-getter id-or-db (cons id2-or-id more)))))

(defn get-cached-coll-prop
  "Uses get-cached-coll to retrieve a map with keys being IDs and values being
  requested properties. If there is no data for the given ID, corresponding entry is
  not added to the resulting map. If the property does not exist, nil is added."
  {:arglists '([cache db-getter db           property ids]
               [cache table     db           property ids]
               [cache table     db-getter db property ids])}
  ([cache db-getter-or-table db prop ids]
   (let [prop (keyword prop)
         ids  (get-cached-coll cache db-getter-or-table db ids)]
     (reduce-kv #(if (nil? %3) (dissoc %1 %2) (qassoc %1 %2 (get %3 prop))) ids ids)))
  ([cache table db-getter db prop ids]
   (let [prop (keyword prop)
         ids  (get-cached-coll cache table db-getter db ids)]
     (reduce-kv #(if (nil? %3) (dissoc %1 %2) (qassoc %1 %2 (get %3 prop))) ids ids))))

(defn get-cached-prop
  "Same as get-cached but retrieves a single property from the result by using the get
  function. When multiple IDs are given it calls get-cached-coll-prop to handle it."
  {:arglists '([cache db-getter db property id]
               [cache db-getter db property id & more]
               [cache table db property id]
               [cache table db property id & more]
               [cache table db-getter db property id]
               [cache table db-getter db property id & more])}
  ([cache db-getter-or-table db prop id]
   (get (get-cached cache db-getter-or-table db id) (keyword prop)))
  ([cache db-getter-or-table db-or-getter prop-or-db id-or-prop id2-or-id]
   (if (data-source? db-or-getter)
     (get-cached-coll-prop cache db-getter-or-table db-or-getter prop-or-db [id-or-prop id2-or-id])
     (get (get-cached cache db-getter-or-table db-or-getter prop-or-db id2-or-id) (id-from-db id-or-prop))))
  ([cache db-getter-or-table db-or-getter prop-or-db id-or-prop id2-or-id & more]
   (if (data-source? db-or-getter)
     (get-cached-coll-prop cache db-getter-or-table db-or-getter prop-or-db (list* id-or-prop id2-or-id more))
     (get-cached-coll-prop cache db-getter-or-table db-or-getter prop-or-db id-or-prop (cons id2-or-id more)))))

(defn get-cached-prop-or-default
  "Same as get-cached-prop but when there is no entry for the given ID, it returns the
  given default value."
  {:arglists '([cache db-getter db property default id]
               [cache db-getter db property default id & more]
               [cache table db property default id]
               [cache table db property default id & more]
               [cache table db-getter db property default id]
               [cache table db-getter db property default id & more])}
  ([cache db-getter-or-table db prop default id]
   (if-some [props (get-cached cache db-getter-or-table db id)]
     (get props (keyword prop))
     default))
  ([cache db-getter-or-table db-or-getter prop-or-db default-or-prop id-or-default id2-or-id]
   (if (data-source? db-or-getter)
     (let [ids (map id-from-db [id-or-default id2-or-id])
           m   (get-cached-coll-prop cache db-getter-or-table db-or-getter prop-or-db ids)]
       (reduce #(if (contains? %1 %2) %1 (qassoc %1 %2 default-or-prop)) m ids))
     (if-some [props (get-cached cache db-getter-or-table db-or-getter prop-or-db id2-or-id)]
       (get props (id-from-db default-or-prop))
       id-or-default)))
  ([cache db-getter-or-table db-or-getter prop-or-db default-or-prop id-or-default id2-or-id & more]
   (if (data-source? db-or-getter)
     (let [ids (cons id-or-default (cons id2-or-id more))
           m   (get-cached-coll-prop cache db-getter-or-table db-or-getter prop-or-db ids)]
       (reduce #(if (contains? %1 %2) %1 (qassoc %1 %2 default-or-prop)) m ids))
     (let [ids (cons id2-or-id more)
           m   (get-cached-coll-prop cache db-getter-or-table db-or-getter prop-or-db default-or-prop ids)]
       (reduce #(if (contains? %1 %2) %1 (qassoc %1 %2 id-or-default)) m ids)))))

;; SQL helpers

(defn for-replace
  "Given a table name and a hash map of column names and their values,
  return a vector of the full `REPLACE` SQL string and its parameters.
  Applies any `:table-fn` / `:column-fn` supplied in the options.  If `:suffix` is
  provided in `opts`, that string is appended to the `INSERT ...` statement."
  [table key-map opts]
  (let [entity-fn (:table-fn opts identity)
        params    (builder/as-keys key-map opts)
        places    (builder/as-? key-map opts)]
    (assert (seq key-map) "key-map may not be empty")
    (into [(str "REPLACE INTO " (entity-fn (name table))
                " (" params ")"
                " VALUES (" places ")"
                (if-let [suffix (:suffix opts)] (str " " suffix)))]
          (vals key-map))))

(defn for-insert-or
  "Given a table name and a hash map of column names and their values,
  return a vector of the full `INSERT OR IGNORE` SQL string and its parameters.
  Applies any `:table-fn` / `:column-fn` supplied in the options.  If `:suffix` is
  provided in `opts`, that string is appended to the `INSERT ...` statement. If
  `:alt-clause` is provided in `opts`, it will replace the default IGNORE string."
  [table key-map opts]
  (let [entity-fn  (:table-fn opts identity)
        alt-clause (:alt-clause opts "IGNORE")
        params     (builder/as-keys key-map opts)
        places     (builder/as-? key-map opts)]
    (assert (seq key-map) "key-map may not be empty")
    (into [(str "INSERT " alt-clause " INTO " (entity-fn (name table))
                " (" params ")"
                " VALUES (" places ")"
                (if-let [suffix (:suffix opts)] (str " " suffix)))]
          (vals key-map))))

(defn for-replace-multi
  "Given a table name, a vector of column names, and a vector of row values
  (each row is a vector of its values), return a vector of the full `REPLACE` SQL
  string and its parameters.  Applies any `:table-fn` / `:column-fn` supplied in the
  options.  If `:suffix` is provided in `opts`, that string is appended to the
  `REPLACE ...` statement."
  [table cols rows opts]
  (assert (apply = (count cols) (map count rows))
          "column counts are not consistent across cols and rows")
  (assert (seq cols) "cols may not be empty")
  (assert (seq rows) "rows may not be empty")
  (let [table-fn  (:table-fn opts identity)
        column-fn (:column-fn opts identity)
        params    (str/join ", " (map (comp column-fn name) cols))
        places    (builder/as-? (first rows) opts)]
    (into [(str "REPLACE INTO " (table-fn (name table))
                " (" params ")"
                " VALUES "
                (str/join ", " (repeat (count rows) (str "(" places ")")))
                (if-let [suffix (:suffix opts)] (str " " suffix)))]
          cat
          rows)))

(defn for-insert-multi-or
  "Given a table name, a vector of column names, and a vector of row values
  (each row is a vector of its values), return a vector of the full `INSERT` SQL
  string and its parameters.  Applies any `:table-fn` / `:column-fn` supplied in the
  options.  If `:suffix` is provided in `opts`, that string is appended to the
  `INSERT IGNORE ...` statement. The `IGNORE` part can be replaced by
  supplying :alt-clause option key."
  [table cols rows opts]
  (assert (apply = (count cols) (map count rows))
          "column counts are not consistent across cols and rows")
  (assert (seq cols) "cols may not be empty")
  (assert (seq rows) "rows may not be empty")
  (let [table-fn   (:table-fn opts identity)
        column-fn  (:column-fn opts identity)
        alt-clause (:alt-clause opts "IGNORE")
        params     (str/join ", " (map (comp column-fn name) cols))
        places     (builder/as-? (first rows) opts)]
    (into [(str "INSERT " alt-clause " INTO " (table-fn (name table))
                " (" params ")"
                " VALUES "
                (str/join ", " (repeat (count rows) (str "(" places ")")))
                (if-let [suffix (:suffix opts)] (str " " suffix)))]
          cat
          rows)))

(defn insert-or!
  "Syntactic sugar over `execute-one!` to make inserting hash maps easier.
  Given a connectable object, a table name, and a data hash map, inserts the data as
  a single row in the database and attempts to return a map of generated keys. By
  default it uses `INSERT OR IGNORE` but the `IGNORE` can be changed to anything by
  supplying :alt-clause option in opts map."
  ([connectable table key-map]
   (insert-or! connectable table key-map {:alt-clause "IGNORE"}))
  ([connectable table key-map opts]
   (let [opts (merge (:options connectable) opts)]
     (jdbc/execute-one! connectable
                        (for-insert-or table key-map opts)
                        (merge {:return-keys true} opts)))))

(defn insert-multi-or!
  "Syntactic sugar over `execute!` to make inserting columns/rows easier.
  Same as insert-or! but supports multiple rows to be inserted at once."
  ([connectable table cols rows]
   (insert-multi-or! connectable table cols rows {:alt-clause "IGNORE"}))
  ([connectable table cols rows opts]
   (if (seq rows)
     (let [opts (merge (:options connectable) opts)]
       (jdbc/execute! connectable
                      (for-insert-multi-or table cols rows opts)
                      (merge {:return-keys true} opts)))
     [])))

(defn insert-or-replace-multi!
  "Syntactic sugar over `execute!` to make inserting columns/rows easier.
  Same as insert-multi! but supports :alt-clause option key."
  ([connectable table cols rows]
   (insert-multi-or! connectable table cols rows {:alt-clause "REPLACE"}))
  ([connectable table cols rows opts]
   (insert-multi-or! connectable table cols rows (qassoc opts :alt-clause "REPLACE"))))

(defn insert-or-ignore-multi!
  "Syntactic sugar over `execute!` to make inserting columns/rows easier.
  Same as insert-multi! but supports :alt-clause option key."
  ([connectable table cols rows]
   (insert-multi-or! connectable table cols rows {:alt-clause "IGNORE"}))
  ([connectable table cols rows opts]
   (insert-multi-or! connectable table cols rows (qassoc opts :alt-clause "IGNORE"))))

(defn insert-or-replace!
  "Syntactic sugar over `execute-one!` to make inserting hash maps easier.
  Given a connectable object, a table name, and a data hash map, inserts the data as
  a single row in the database and attempts to return a map of generated keys."
  ([connectable table key-map]
   (insert-or! connectable table key-map {:alt-clause "REPLACE"}))
  ([connectable table key-map opts]
   (insert-or! connectable table key-map (qassoc opts :alt-clause "REPLACE"))))

(defn insert-or-ignore!
  "Syntactic sugar over `execute-one!` to make inserting hash maps easier.
  Given a connectable object, a table name, and a data hash map, inserts the data as
  a single row in the database and attempts to return a map of generated keys."
  ([connectable table key-map]
   (insert-or! connectable table key-map {:alt-clause "IGNORE"}))
  ([connectable table key-map opts]
   (insert-or! connectable table key-map (qassoc opts :alt-clause "IGNORE"))))

(defn replace!
  "Syntactic sugar over `execute-one!` to make inserting hash maps easier.
  Given a connectable object, a table name, and a data hash map, inserts the data as
  a single row in the database and attempts to return a map of generated keys. By
  default it uses `REPLACE`."
  ([connectable table key-map]
   (replace! connectable table key-map {}))
  ([connectable table key-map opts]
   (let [opts (merge (:options connectable) opts)]
     (jdbc/execute-one! connectable
                        (for-replace table key-map opts)
                        (merge {:return-keys true} opts)))))

(defn replace-multi!
  "Syntactic sugar over `execute!` to make inserting columns/rows easier.
  Same as replace! but supports multiple rows to be inserted at once."
  ([connectable table cols rows]
   (replace-multi! connectable table cols rows {}))
  ([connectable table cols rows opts]
   (if (seq rows)
     (let [opts (merge (:options connectable) opts)]
       (jdbc/execute! connectable
                      (for-replace-multi table cols rows opts)
                      (merge {:return-keys true} opts)))
     [])))

;; Settings abstraction

(def listed-nil '(nil))

(defn- prep-pairs
  [coll]
  (if (coll? coll)
    (seq (if (map? coll) (mapcat seq coll) coll))))

(defn- prep-names
  [coll]
  (if (coll? coll)
    (seq (if (map? coll) (keys coll) coll))))

(defn get-failed?
  "Returns true if getting from a database failed in post-processing
  phase (e.g. de-serialization) and the data were broken."
  [v]
  (identical? ::get-failed v))

(defn make-setting-getter
  "Returns a function which gets a setting for the given entity and de-serializes it to
  a Clojure data structure. Table name and entity column name must be quoted if
  needed before passing to this function."
  [table entity-column]
  (let [table           (to-snake-simple table)
        entity-column   (to-snake-simple entity-column)
        ret-value-key   (keyword (name table) "value")
        getter-query    (str-spc "SELECT value FROM" table
                                 "WHERE" entity-column "= ? AND id = ?")
        getter-subquery (str-spc "SELECT id, value FROM" table
                                 "WHERE" entity-column "= ? AND id IN")]
    (fn get-setting
      ([db setting-fq]
       (get-setting db (namespace setting-fq) (name setting-fq)))
      ([db entity-id setting-id]
       (if-some [entity-id (id-to-db entity-id)]
         (if-some [setting-id (some-str setting-id)]
           (if db
             (if-some [r (ret-value-key
                          (jdbc/execute-one! db [getter-query entity-id setting-id]))]
               (try
                 (nippy/thaw r)
                 (catch Throwable e
                   (log/err "Error de-serializing setting" setting-id "for" entity-id "in" table)
                   (log/dbg e)
                   ::get-failed)))
             (log/err "Cannot get setting" setting-id "from" table "for" entity-id
                      "because database connection is not set")))))
      ([db entity-id setting-id & setting-ids]
       (if-let [setting-ids (prep-names setting-ids)]
         (if-some [entity-id (id-to-db entity-id)]
           (if-some [setting-id (some-str setting-id)]
             (if-not db
               (log/err "Cannot get settings from" table "for" entity-id
                        "because database connection is not set")
               (let [setting-ids (cons setting-id (map id-to-db setting-ids))
                     query       (cons (str-spc getter-subquery (braced-join-? setting-ids))
                                       (cons entity-id setting-ids))
                     results     (next (jdbc/execute! db query opts-simple-vec))]
                 (if results
                   (try
                     (reduce (fn [m [id v]]
                               (try
                                 (qassoc m (keyword id) (nippy/thaw v))
                                 (catch Throwable e
                                   (log/err "Error de-serializing setting" id "for" entity-id "in" table)
                                   (throw e)))) {} results)
                     (catch Throwable e
                       (log/dbg e)
                       ::get-failed)))))))
         (get-setting db entity-id setting-id))))))

(defn make-setting-setter
  "Returns a function which stores one or more settings for a given entity in a
  database. Max. object size is 32 KB. Table name and entity column name must be
  quoted if needed before passing to this function."
  [table entity-column]
  (let [table         (some-keyword-simple table)
        entity-column (some-keyword-simple entity-column)]
    (fn put-setting
      ([db entity-id setting-id value]
       (if-some [entity-id (id-to-db entity-id)]
         (if-some [setting-id (some-str setting-id)]
           (if-not db
             (log/err "Cannot store setting" setting-id "in" table "for" entity-id
                      "because database connection is not set")
             (or (pos-int?
                  (::jdbc/update-count
                   (replace! db table
                             {:id           setting-id
                              entity-column entity-id
                              :value        (nippy/freeze value)}
                             opts-map)))
                 (log/err "Error putting setting" setting-id
                          "into a database table" table))))))
      ([db entity-id setting-id value & pairs]
       (if-let [pairs (prep-pairs pairs)]
         (if-some [entity-id (id-to-db entity-id)]
           (if-some [setting-id (some-str setting-id)]
             (if-not db
               (log/err "Cannot store setting" setting-id "in" table "for" entity-id
                        "because database connection is not set")
               (let [r (replace-multi!
                        db table
                        [entity-column :id :value]
                        (->> pairs (cons value) (cons setting-id)
                             (partition 2 2 listed-nil)
                             (map (juxt-seq (constantly entity-id)
                                            (comp id-to-db first)
                                            (comp nippy/freeze second))))
                        opts-map)]
                 (or (pos-int? (::jdbc/update-count (first r)))
                     (log/err "Error putting settings into a database table" table
                              "for" entity-id))))))
         (put-setting db entity-id setting-id value))))))

(defn make-setting-deleter
  "Creates a setting deleter on a basis of the given table and entity column."
  [table entity-column]
  (let [table-str         (to-snake-simple table)
        table             (some-keyword-simple table-str)
        entity-column-str (to-snake-simple entity-column)
        entity-column     (some-keyword-simple entity-column-str)
        ret-subquery      "RETURNING id"
        deleter-subquery  (str-spc "DELETE FROM" table-str
                                   "WHERE" entity-column-str "= ? AND id IN ")
        db-opts           (qassoc opts-simple-map :return-keys false)
        db-opts-ret       (-> opts-simple-vec
                              (qassoc :return-keys false)
                              (qassoc :suffix ret-subquery))]
    (fn del-setting
      ([db entity-id]
       (if-some [entity-id (id-to-db entity-id)]
         (if-not db
           (log/err "Cannot delete settings in" table "for" entity-id
                    "because database connection is not set")
           (sql/delete! db table {entity-column entity-id} db-opts-ret))))
      ([db entity-id setting-id]
       (if-some [entity-id (id-to-db entity-id)]
         (if-some [setting-id (some-str setting-id)]
           (if-not db
             (log/err "Cannot delete setting" setting-id "in" table "for" entity-id
                      "because database connection is not set")
             (-> (sql/delete! db table {:id setting-id entity-column entity-id} db-opts)
                 ::jdbc/update-count
                 pos-int?)))))
      ([db entity-id setting-id & setting-ids]
       (if-let [setting-ids (prep-names setting-ids)]
         (if-some [entity-id (id-to-db entity-id)]
           (if-some [setting-id (some-str setting-id)]
             (if-not db
               (log/err "Cannot delete settings in" table "for" entity-id
                        "because database connection is not set")
               (let [setting-ids (mapv some-str (cons setting-id setting-ids))
                     valsqls     (str "(" (str/join "," (repeat (count setting-ids) "?")) ")")
                     query       (cons (str-spc deleter-subquery valsqls) (cons entity-id setting-ids))]
                 (-> (jdbc/execute-one! db query db-opts)
                     ::jdbc/update-count
                     pos-int?)))))
         (del-setting db entity-id setting-id))))))

;; Cached settings

(defn cached-setting-get
  "Gets the cached result of calling the given setting getter. Updates cache when
  necessary."
  ([cache getter db entity-id setting-id]
   (cwr/lookup-or-miss cache (mk-keyword entity-id setting-id) #(getter db %)))
  ([cache getter db entity-id setting-id & setting-ids]
   (if-let [setting-ids (prep-names setting-ids)]
     (let [entity-id    (id-as-str entity-id)
           setting-ids  (map id-as-str (cons setting-id setting-ids))
           lookup-ids   (map #(keyword entity-id %) setting-ids)
           looked-up-fq (cache-lookup-coll cache lookup-ids)
           found        (map/map-keys (comp keyword name) (dissoc looked-up-fq ::not-found))
           not-found    (map (comp keyword name) (get looked-up-fq ::not-found))]
       (if-not (seq not-found)
         found
         (let [from-db (apply getter db entity-id not-found)]
           (reduce #(qassoc %1 %2 (cwr/lookup-or-miss cache (mk-keyword entity-id %2)
                                                      (comp from-db keyword name)))
                   found not-found))))
     (cached-setting-get cache getter db entity-id setting-id))))

(defn cached-setting-set
  "Sets the cached result of calling the given setting setter. Purges cache entry after
  operation succeeded."
  ([cache setter db entity-id setting-id value]
   (let [r (setter db entity-id setting-id value)]
     (cache-evict! cache (mk-keyword entity-id setting-id)) r))
  ([cache setter db entity-id setting-id value & pairs]
   (let [r         (apply setter db entity-id setting-id value pairs)
         entity-id (id-as-str entity-id)]
     (apply cache-evict! cache (mk-keyword entity-id setting-id)
            (map #(mk-keyword entity-id %) (take-nth 2 pairs)))
     r)))

(defn cached-setting-del
  "Deletes the cached result of calling the given setting deleter. Purges cache entry
  after operation succeeded."
  ([cache deleter db entity-id]
   (if-some [r (deleter db entity-id)]
     (let [entity-id (id-as-str entity-id)]
       (apply cache-evict! cache (map #(mk-keyword entity-id %) r)) true)))
  ([cache deleter db entity-id setting-id]
   (let [r (deleter db entity-id setting-id)]
     (cache-evict! cache (mk-keyword entity-id setting-id)) r))
  ([cache deleter db entity-id setting-id & pairs]
   (let [r         (apply deleter db entity-id setting-id pairs)
         entity-id (id-as-str entity-id)]
     (apply cache-evict! cache
            (mk-keyword entity-id setting-id)
            (map #(mk-keyword entity-id %) (take-nth 2 pairs)))
     r)))

(defn init-cache
  "Initializes single cache by parsing TTL and queue size."
  [{:keys [size ttl seed]}]
  (cache-prepare (time/parse-duration ttl)
                 (safe-parse-long size)
                 (or seed {})))

(defn init-caches
  "Initializes in-memory caches by resetting atoms associated with cache parameters."
  [config]
  (log/msg "Initializing abstract in-memory caches" (str "(" (count config) " slots)"))
  (reduce-kv
   (fn [acc id cfg]
     (let [cache-atom (var/deref id)
           cache-atom (if (atom? cache-atom) cache-atom (var/make id (atom nil)))
           cfg        (if (map? cfg) cfg {})]
       (reset! cache-atom (init-cache cfg))
       (qassoc acc (symbol id) cache-atom)))
   {} config))

(defn remove-caches
  "Removes cache structures."
  [config]
  (log/msg "Removing abstract in-memory caches")
  (doseq [c (vals config)]
    (if (atom? c)
      (reset! c nil)))
  nil)

(defn print-caches
  [caches]
  (doseq [[id c] caches]
    (let [c     (if (atom? c) (deref c) c)
          t     (peek (str/split (pr-str (type c)) #"\."))
          cfg   (get-in (var/deref 'app/config) [::caches (keyword id)])
          ttl   (some->> cfg :ttl (str/join " ") (str ", TTL: "))
          qsize (some->> cfg :size (str ", Size: "))]
      (println (str id " (" t ttl qsize "): "
                    (count c) " entries.")))))
