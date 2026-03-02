(ns

    ^{:doc    "Random Utilities, databases coercers."
      :author "Paweł Wilk"
      :added  "2.0.9"}

    io.randomseed.utils.db.coercion

  (:refer-clojure :exclude [memoize parse-long uuid random-uuid -> <-])

  (:require [clojure.core                  :as                      c]
            [next.jdbc                     :as                   jdbc]
            [next.jdbc.sql                 :as               next-sql]
            [next.jdbc.result-set          :as                     rs]
            [potemkin                      :as                      p]
            [io.randomseed.utils           :as                      u]
            [io.randomseed.utils.db        :as                     db]
            [io.randomseed.utils.db.types  :as               db-types]
            [io.randomseed.utils.map       :as                    map]
            [io.randomseed.utils.db.sql    :as                    sql]
            [io.randomseed.utils.identity  :as               identity])

  (:import (java.sql ResultSet ResultSetMetaData)))

(set! *warn-on-reflection* true)

;; Database column readers and result set setters

(db-types/add-all-readers)
(db-types/add-all-setters)

;; Type checks

(p/import-vars [io.randomseed.utils.db data-source?])

;; Memoization

(p/import-vars [io.randomseed.utils.db
                memoize memoize+ memoizer invalidate! invalidate+! invalidator])

;; Generic getters and setters

(p/import-vars [io.randomseed.utils.db
                make-setter make-deleter
                get-ids get-id not-found?])

;; Cached database access

(p/import-vars [io.randomseed.utils.db
                cache-prepare cache-create cache-evict! cache-lookup-coll cache-lookup
                get-cached-coll get-cached get-cached-coll-prop
                get-cached-prop get-cached-prop-or-default])

;; SQL helpers

(p/import-vars [io.randomseed.utils.db
                for-replace for-insert-or for-replace-multi for-insert-multi-or
                insert-or! insert-multi-or!
                insert-or-replace-multi! insert-or-ignore-multi!
                insert-or-replace! insert-or-ignore!
                replace! replace-multi!])

;; Database result processing helpers

(p/import-vars [io.randomseed.utils.db get-failed? id-from-db id-to-db])

;; Settings abstraction

(p/import-vars [io.randomseed.utils.db make-setting-getter make-setting-setter make-setting-deleter])

;; Coercion

(defmulti in-coercer
  "Returns a coercer suitable for transforming the given argument `v` to a
  database-suitable value, assuming table and column specified by the given qualified
  keyword `table-column`."
  {:arglists '([^Keyword table-column v] [^Keyword column v])}
  identity)

(defmulti out-coercer
  "Returns a coercer suitable for transforming the given argument `v` read from a
  database, assuming table and column specified by the given qualified keyword
  `table-column`."
  {:arglists '([^Keyword table-column v] [^Keyword column v])}
  identity)

(defmethod in-coercer  :default [_] nil)
(defmethod out-coercer :default [_] nil)

(defn get-in-coercer*
  "Same as `get-in-coercer` but trusts that `table-column` is a fully-qualified
  keyword (already a result of `colspec-kw`)."
  {:see-also ["get-in-coercer"]}
  [table-column]
  (if-some [f (in-coercer table-column)] f
           (when (namespace table-column)
             (in-coercer (keyword (name table-column))))))

(defn get-in-coercer
  "Tries to obtain a database coercion function by calling `in-coercer` multimethod for
  column and table specified with `table` and `column`, or by a single
  `table-column`. Transforms arguments to a lisp-cased keyword. If there is no
  coercer for table and column, tries to getting one using the column alone.

  Returns coercer when it is found. Returns `false` when a coercer is found but
  explicitly set to undefined. Returns `nil` when there is no coercer."
  ([table column]
   (if-some [f (in-coercer (sql/colspec-kw table column))] f (in-coercer (sql/column-kw column))))
  ([table-column]
   (get-in-coercer* (sql/colspec-kw table-column))))

(defn get-out-coercer*
  "Same as `get-out-coercer` but trusts that `table-column` is a lisp-cased
  keyword (already a result of `colspec-kw`)."
  {:see-also ["get-out-coercer"]}
  [table-column]
  (if-some [f (out-coercer table-column)] f
           (when (namespace table-column)
             (out-coercer (keyword (name table-column))))))

(defn get-out-coercer
  "Tries to obtain a database coercion function by calling `out-coercer` multimethod
  for column and table specified with `table` and `column`, or by a single
  `table-column`. Transforms arguments to a lisp-cased keyword. If there is no
  coercer for table and column, falls back to getting one using the column alone.

  Returns coercer when it is found. Returns `false` when a coercer is found but
  explicitly set to undefined. Returns `nil` when there is no coercer."
  ([table column]
   (if-some [f (out-coercer (sql/colspec-kw table column))] f (out-coercer (sql/column-kw column))))
  ([table-column]
   (get-out-coercer* (sql/colspec-kw table-column))))

(defn- statically-convertable?
  "Returns `true` if `v` can be statically handled at compile-time, `false` otherwise."
  ([v]
   (or (nil? v) (keyword? v) (string? v) (number? v) (boolean? v)))
  ([v ts cs]
   (and (string? ts) (string? cs)
        (or (nil? v) (keyword? v) (string? v) (number? v) (boolean? v)))))

(defmacro literal-result
  "Returns a value of the given argument `v` if it is statically convertable value (and
  it is ok to be put into a source code in literal form) or a function. Otherwise
  returns `alt` or `nil` (if `alt` is not given)."
  {:no-doc true}
  ([v]     `(let [v# ~v] (if (or (fn? v#) (statically-convertable? v#)) v#)))
  ([v alt] `(let [v# ~v] (if (or (fn? v#) (statically-convertable? v#)) v# ~alt))))

(defn coerce-in
  "Coerces the given value `v` to a database type by calling a function returned by
  invoking `io.randomseed.utils.db/in-coercer` multimethod on a qualified keyword
  `table-column` (or a qualified keyword made out of `table` and `column`). If there
  is no coercer attached for the keyword, tries with a keyword created using the
  `column-kw` function. If there is no coercer, returns unchanged `v`.

  Will immediately return the given value `v` if the coercer exists but it is set to
  `false`."
  ([table column v] (if-let [f (get-in-coercer table column)] (f v) v))
  ([table-column v] (if-let [f (get-in-coercer table-column)] (f v) v)))

(defn coerce-in*
  "Same as `coerce-in` but `table-column` must be a lisp-cased keyword (already a
  result of `colspec-kw` or `column-kw`)."
  [table-column v] (if-let [f (get-in-coercer* table-column)] (f v) v))

(defn coerce-out
  "Coerces the given value `v` from a database type by calling a function returned by
  invoking `io.randomseed.utils.db/out-coercer` multimethod on a qualified keyword
  `table-column` (or a qualified keyword made out of `table` and `column`). If there
  is no coercer attached for the keyword, tries with a keyword created using the
  `column-kw` function (to use column alone). If there is no coercer, returns
  unchanged `v`.

  Will immediately return the given value `v` if the coercer exists but it is
  set to `false`."
  ([table column v] (if-let [f (get-out-coercer table column)] (f v) v))
  ([table-column v] (if-let [f (get-out-coercer table-column)] (f v) v)))

(defn coerce-out*
  "Same as `coerce-out` but `table-column` must be a lisp-cased keyword (already a
  result of `colspec-kw` or `column-kw`)."
  [table-column v] (if-let [f (get-out-coercer* table-column)] (f v) v))

(defn coerce-seq-in
  "Coerces a sequence of values `coll` to database types by calling a function returned
  by invoking `io.randomseed.utils.db/in-coercer` multimethod on a qualified keyword
  `table-column` (or a qualified keyword made out of `table` and `column`). If there
  is no coercer attached for the keyword, tries with a keyword created using the
  `column-kw` function (to use column alone). If there is no coercer, returns
  unchanged `coll`."
  ([table column coll] (if-let [f (get-in-coercer table column)] (map f coll) coll))
  ([table-column coll] (if-let [f (get-in-coercer table-column)] (map f coll) coll)))

(defn coerce-seq-in*
  "Same as `coerce-seq-in` but `table-column` must be a lisp-cased keyword (already a
  result of `colspec-kw` or `column-kw`)."
  [table-column coll] (if-let [f (get-in-coercer* table-column)] (map f coll) coll))

(defn coerce-seq-out
  "Coerces a sequence of values `coll` from database types by calling a function
  returned by invoking `io.randomseed.utils.db/out-coercer` multimethod on a qualified keyword
  `table-column` (or a qualified keyword made out of `table` and `column`). If there
  is no coercer attached for the keyword, tries with a keyword created using the
  `column-kw` function (to use column alone). If there is no coercer, returns
  unchanged `coll`."
  ([table column coll] (if-let [f (get-out-coercer table column)] (map f coll) coll))
  ([table-column coll] (if-let [f (get-out-coercer table-column)] (map f coll) coll)))

(defn coerce-seq-out*
  "Same as `coerce-seq-out` but `table-column` must be a lisp-cased keyword (already a
  result of `colspec-kw` or `column-kw`)."
  [table-column coll] (if-let [f (get-out-coercer* table-column)] (map f coll) coll))

(defmacro <-
  "Coerces value `v` to a database type by calling a function returned by invoking
  `io.randomseed.utils.db/in-coercer` multimethod on a qualified keyword `table-column` (or a
  qualified keyword made out of `table` and `column`). If there is no coercer
  attached for the keyword, returns unchanged `v`.

  If a coercer can be obtained at compile-time, a coercion function-call form will be
  generated. If a coercer can be obtained at compile-time and the given value is
  statically convertable, value resulting from applying coercion function will be
  generated immediately."
  ([table column v]
   (if (and (or (keyword? table)  (string? table))
            (or (keyword? column) (string? column)))
     (let [tc (literal-result (sql/colspec-kw table column))]
       (if-some [coercer-fn (literal-result (get-in-coercer* tc))]
         (if coercer-fn
           (if (statically-convertable? v)
             (literal-result (coercer-fn v) (list `~coercer-fn `~v))
             (list `~coercer-fn `~v))
           `~v)
         `(coerce-in* ~tc ~v)))
     `(coerce-in ~table ~column ~v)))
  ([table-column v]
   (if (or (keyword? table-column) (string? table-column))
     (let [tc (literal-result (sql/colspec-kw table-column))]
       (if-some [coercer-fn (literal-result (get-in-coercer* tc))]
         (if coercer-fn
           (if (statically-convertable? v)
             (literal-result (coercer-fn v) (list `~coercer-fn `~v))
             (list `~coercer-fn `~v))
           `~v)
         `(coerce-in* ~tc ~v)))
     `(coerce-in ~table-column ~v))))

(defn ->
  "Coerces value `v` from a database type by calling a function returned by invoking
  `io.randomseed.utils.db/out-coercer` multimethod on a qualified keyword `table-column` (or a
  qualified keyword made out of `table` and `column`). If there is no coercer
  attached for the keyword, returns unchanged `v`.

  If a coercer can be obtained at compile-time, a coercion function-call form will be
  generated. If a coercer can be obtained at compile-time and the given value is
  statically convertable, value resulting from applying coercion function will be
  generated immediately."
  ([table column v]
   (if (and (or (keyword? table)  (string? table))
            (or (keyword? column) (string? column)))
     (let [tc (literal-result (sql/colspec-kw table column))]
       (if-some [coercer-fn (literal-result (get-out-coercer* tc))]
         (if coercer-fn
           (if (statically-convertable? v)
             (literal-result (coercer-fn v) (list `~coercer-fn `~v))
             (list `~coercer-fn `~v))
           `~v)
         `(coerce-out* ~tc ~v)))
     `(coerce-out ~table ~column ~v)))
  ([table-column v]
   (if (or (keyword? table-column) (string? table-column))
     (let [tc (literal-result (sql/colspec-kw table-column))]
       (if-some [coercer-fn (literal-result (get-out-coercer* tc))]
         (if coercer-fn
           (if (statically-convertable? v)
             (literal-result (coercer-fn v) (list `~coercer-fn `~v))
             (list `~coercer-fn `~v))
           `~v)
         `(coerce-out* ~tc ~v)))
     `(coerce-out ~table-column ~v))))

(defmacro <-seq
  "Coerces a sequence of values `coll` to database types by calling a function returned
  by invoking `io.randomseed.utils.db/in-coercer` multimethod on a qualified keyword
  `table-column` (or a qualified keyword made out of `table` and `column`). If there
  is no coercer attached for the keyword, returns unchanged `coll`.

  If both, a table and a column can be used to establish coercion function at
  compile-time, a mapping form will be generated which uses that function."
  ([table column coll]
   (if (and (or (keyword? table)  (string? table))
            (or (keyword? column) (string? column)))
     (let [tc (literal-result (sql/colspec-kw table column))]
       (if-some [coercer-fn (literal-result (get-in-coercer* tc))]
         (if coercer-fn (list `map `~coercer-fn `~coll) `~coll)
         `(coerce-seq-in* ~tc ~coll)))
     `(coerce-seq-in ~table ~column ~coll)))
  ([table-column coll]
   (if (or (keyword? table-column) (string? table-column))
     (let [tc (literal-result (sql/colspec-kw table-column))]
       (if-some [coercer-fn (literal-result (get-in-coercer* tc))]
         (if coercer-fn (list `map `~coercer-fn `~coll) `~coll)
         `(coerce-seq-in* ~tc ~coll)))
     `(coerce-seq-in ~table-column ~coll))))

(defmacro seq->
  "Coerces a sequence of values `coll` from database types by calling a function
  returned by invoking `io.randomseed.utils.db/out-coercer` multimethod on a qualified keyword
  `table-column` (or a qualified keyword made out of `table` and `column`). If there
  is no coercer attached for the keyword, returns unchanged `coll`.

  If both, a table and a column can be used to establish coercion function at
  compile-time, a mapping form will be generated which uses that function."
  ([table column coll]
   (if (and (or (keyword? table)  (string? table))
            (or (keyword? column) (string? column)))
     (let [tc (literal-result (sql/colspec-kw table column))]
       (if-some [coercer-fn (literal-result (get-out-coercer* tc))]
         (if coercer-fn (list `map `~coercer-fn `~coll) `~coll)
         `(coerce-seq-out* ~tc ~coll)))
     `(coerce-seq-out ~table ~column ~coll)))
  ([table-column coll]
   (if (or (keyword? table-column) (string? table-column))
     (let [tc (literal-result (sql/colspec-kw table-column))]
       (if-some [coercer-fn (literal-result (get-out-coercer* tc))]
         (if coercer-fn (list `map `~coercer-fn `~coll) `~coll)
         `(coerce-seq-out* ~tc ~coll)))
     `(coerce-seq-out ~table-column ~coll))))

(defrecord QSlot [^String t ^String c v])

(defn- not-empty-qslot?
  "Returns `true` if the given `e` is of type `QSlot` and all of its essential fields
  are not empty."
  [e]
  (and (instance? QSlot e) (.t ^QSlot e) (.c ^QSlot e) (.v ^QSlot e)))

(defn- join-qslots
  "Joins consecutive `QSlot` records if their column and table fields are equal."
  [done qs]
  (let [prev (peek done)]
    (if (and (not-empty-qslot? qs) (not-empty-qslot? prev))
      (if (and (= (.t ^QSlot prev) (.t ^QSlot qs))
               (= (.c ^QSlot prev) (.c ^QSlot qs)))
        (conj (pop done) (map/qupdate prev :v into (.v ^QSlot qs)))
        (conj done qs))
      (conj done qs))))

(defn- c-t
  "Extracts column and table specs as a vector from the given control keyword and
  predefined hints for table and column spec."
  [ctrl table-spec column-spec]
  (when (keyword? ctrl)
    (let [[c t] (sql/column-table ctrl)]
      (if c
        (if t [c t] (if table-spec [c table-spec] [table-spec c]))
        [column-spec table-spec]))))

(defn- pp-conv-specs
  "Pre-parses table/column conversion specifications."
  ([coll]
   (->> (mapcat #(pp-conv-specs % nil nil) coll)
        (reduce join-qslots [])))
  ([e tspec cspec]
   (cond

     ;; control vector
     ;; (group of values followed by table or column spec)

     (vector? e)
     (let [ctrl (nth e 0 nil), coll (subvec e 1)]
       (if (keyword? ctrl)
         ;; static table/column name
         (let [[c t] (c-t ctrl tspec cspec)]
           (mapcat #(pp-conv-specs % t c) coll))
         ;; dynamic table/column name
         (let [[c t] (if tspec [ctrl tspec] [nil ctrl])]
           (mapcat #(pp-conv-specs % t c) coll))))

     ;; single value
     ;; (known table and column)

     (and tspec cspec (statically-convertable? e tspec cspec))
     (if-some [coercer-fn (literal-result (get-in-coercer tspec cspec))]
       (cons (if coercer-fn (literal-result (coercer-fn e) e) e) nil)
       (cons (QSlot. tspec cspec [e]) nil))

     ;; regular dynamically-convertable element
     ;; (known table and column)

     (and tspec cspec)
     (cons (QSlot. tspec cspec [e]) nil)

     ;; single value expressed with a simple symbol
     ;; (known table, missing column)

     (and tspec (simple-symbol? e))
     (cons (QSlot. tspec (name e) [e]) nil)

     ;; any value

     :else (cons e nil))))

(defn- parse-conv-spec
  "Parses value with optional table/column conversion specification to produce a source
  code. Expects values or `QSlot` records."
  [e]
  (if (or (not (instance? QSlot e)) (nil? (.t ^QSlot e)))
    (cons e nil)
    (let [t  (.t ^QSlot e)
          c  (.c ^QSlot e)
          v  (.v ^QSlot e)
          tc (when (and (or (keyword? t) (string? t))
                        (or (keyword? c) (string? c)))
               (literal-result (sql/colspec-kw t c)))]
      (if (= (count v) 1)
        (if tc
          (cons `(<- ~tc   ~(nth v 0)) nil)
          (cons `(<- ~t ~c ~(nth v 0)) nil))
        (if tc
          (if-some [coercer-fn (literal-result (get-in-coercer* tc))]
            (if coercer-fn (map (fn [e] `(~coercer-fn ~e)) v) `~v)
            (map (fn [e] `(<- ~tc ~e)) v))
          (map (fn [e] `(<- ~t ~c ~e)) v))))))

(defn gen-qs-keyword
  "Generates unique but deterministic symbolic name for `t` (presumably table name),
  `c` (column name) and `v` (value, being an identifier). Returns a keyword named
  like `DB__[t]_[c]_[v]_[nnnnnnn]` where `[t]`, `[c]` and `[v]` are string
  representations of the given argument values, and `[nnnnnnn]` is a numeric
  representation of combined hash of all values given as arguments."
  ([^QSlot qs]   (gen-qs-keyword (.t qs) (.c qs) (.v qs)))
  ([^QSlot qs v] (gen-qs-keyword (.t qs) (.c qs) v))
  ([t c v]
   (let [^String h (c/-> (hash t) (hash-combine (hash c)) (hash-combine (hash v)) u/strb)
         ^String h (if (identical? \- (.charAt h 0)) (u/strb "0" (subs h 1)) h)]
     (keyword (u/strb "DB__" (u/some-str t) "_" (u/some-str c) "_" (u/some-str v) "_" h)))))

(defn- repeating-qslot-bindings
  "Returns a map with keys being keywords representing unique table/column/value names
  identifying repeated, single-valued `QSlot` elements from the given `coll`, and
  with associated values being expressions for performing output database coercion."
  [coll]
  (->> (filter #(instance? QSlot %) coll)
       (remove #(or (nil? (.t ^QSlot %)) (nil? (.c ^QSlot %)) (list? (.t ^QSlot %)) (list? (.c ^QSlot %))))
       (mapcat #(map (fn [v] (map/qassoc % :v v)) (.v ^QSlot %)))
       (frequencies) (seq)
       (filter #(> (val %) 1))
       (map (juxt #(gen-qs-keyword (key %)) #(map/qupdate (key %) :v vector)))
       (into {})
       (map/map-vals (comp first parse-conv-spec))))

(defn bindable-sym
  "Returns a bindable, auto-generated symbol for the table/column (from `qs`, which
  should be a `QSlot` record) and a value `v`. The unique identifier (obtained using
  `gen-qs-keyword`) must exists in `bindings` map. Otherwise, `nil` is returned."
  [bindings ^QSlot qs v]
  (let [h (gen-qs-keyword qs v)]
    (when (contains? bindings h) (symbol h))))

(defn bindable-sym?
  "Returns `true` if a bindable, auto-generated symbol for the table/column (from `qs`,
  which should be a `QSlot` record) and a value `v` exist in `bindings` map. Otherwise
  it returns `false`."
  [bindings ^QSlot qs v]
  (contains? bindings (gen-qs-keyword qs v)))

(defn- replace-bindable
  "Replaces bindable expressions from `:v` fields of `QSlot` records present in `coll`
  by unique symbols corresponding to them, with names created with `gen-qs-keyword`
  which exist as keys in `bindings` map."
  [bindings coll]
  (vec
   (mapcat
    (fn [qs]
      (if (instance? QSlot qs)
        (let [parts    (partition-by #(bindable-sym? bindings qs %) (.v ^QSlot qs))
              first-b? (bindable-sym? bindings qs (ffirst parts))]
          (mapcat (fn [qvals bindable?]
                    (if bindable?
                      (map #(bindable-sym bindings qs %) qvals)
                      (cons (map/qassoc qs :v (vec qvals)) nil)))
                  parts (iterate not first-b?)))
        (cons qs nil)))
    coll)))

(defn- prepend-qslot-bindings
  "Wraps the given `coll` in a `let` block with `bindings`."
  [bindings coll]
  (if-some [bindings (seq bindings)]
    (list `let (vec (mapcat #(update % 0 symbol) bindings)) coll)
    coll))

(defmacro <<-
  "Magical macro which converts a sequence of values with optional table and column
  specifications to a database-suitable formats. Pre-processing of arguments is
  executed at compile-time, further processing is performed at run-time.

  Any type of argument is accepted but literal vectors, including nested vectors, are
  **control structures**. Their first elements are table names (for the first one),
  column names (for nested vectors) or both (when expressed using fully-qualified
  keywords).

  Example: `(<<- 1 2 3 [:table-name [:column-name :val1 val2 (exp3) \"val4\"])`

  `1`, `2` and `3` are regular values, `:table-name` is literally expressed table
  name for coercer, `:column-name` is literally expressed column name for coercer,
  other expressions are values to be coerced. Table and column names can be dynamic,
  expressed with symbols or call forms.

  All macro arguments are sequentially transformed with the following rules:

  - If there is a **literal vector** at **1st level**, its first element should be a
  table name. All elements of that vector will inherit that table name during
  conversion to a database-suitable format.

  - If there is a **literal vector** nested within existing vector its first element
  will be considered a **column name**. All elements of that vector will inherit that
  column name during conversion to a database-suitable format.

  - If the given literal vector contains a **fully-qualified keyword** at its first
  position then both **table** and **column** will be memorized to be applied during
  conversion of elements contained in that vector (table taken from a namespace and
  column from a name of the keyword).

  - If there is a table name inherited but no column specified, a value is not
  converted but returned as is, with the exception: when the value is expressed as a
  **literal, simple symbol** then a column name will be derived from its name.

  - A sub-vector may begin with an unspecified value `nil`. In such case it will
  group values to be converted but column name will not be set. The values will be
  left unconverted unless they are simple symbol forms; in such case column names
  will be derived from their names.

  Values used to set column and table names at the beginnings of vectors can be
  expressed with any valid code. However, literal strings or literal keywords (or
  symbols in case of identifier-derived column names) will be pre-processed at
  compile time. So, if both column and table name are expressed that way, the
  conversion specifier will be generated ahead.

  Moreover, if apart from the above, a value to be coerced is expressed as a literal
  number, string, keyword, boolean, or a `nil`, it will be converted at compile-time.

  Conversion of repeating atomic expressions sharing the same table and column name
  identifiers (including symbolic identifiers) will be performed in ad-hoc created
  `let` block, and the results will be referenced using auto-generated symbols
  replacing the original expressions. Therefore, conversion for repeated symbols (and
  other atomic expressions) will be performed just once.

  Examples:

  `(<<- [:users [:id id] [:email email]])`

  The above will convert values expressed by `id` and `email` symbol forms using a
  variant of coercion multimethod registered for `:users/id` and `:users/email`
  keywords, accordingly.

  `(<<- [:users/id id [:email e]])`

  The above will convert values of `id` and `e` symbol forms using a variant of
  coercion multimethod registered for `:users/id` and `:users/email` keywords,
  accordingly.

  `(<<- [:users [:id id] [:confirmations/email email [:expires expires]])`

  The above will convert values of `id`, `email` and `expires` symbol forms using a
  variant of coercion multimethod registered for `:users/id`, `:confirmations/email`,
  and `:confirmations/expires` keywords, accordingly. We can see that second vector
  changes the table name to `confirmations` in its scope so later `expires` derives
  it and is converted with `:confirmations/expires` specification.

  This is synonymous to:

  `(<<- [:users id] [:confirmations email expires])`

  As we can see, the `id` symbolic identifier is used to set the column name, same as
  `email` and `expires` symbols. The above code will be expanded to:

  ```
  [(io.randomseed.utils.db/<- :users/id id)
   (io.randomseed.utils.db/<- :confirmations/email email)
   (io.randomseed.utils.db/<- :confirmations/expires expires)]
  ```

  And then to:

  ```
  [(#<Fn@4d7e49d ferment.model.user/id_to_db> id)
   (io.randomseed.utils.db/coerce-in* :confirmations/email email)
   (#<Fn@3bf6fdc6 ferment.model.confirmation/to_expiry> expires)]
  ```

  We can see that coercers for `id` and `expires` symbols were resolved and function
  call forms were created at compile-time. That's because `:users/id` and
  `:confirmations/expires` were recognized as existing dispatch values when calling
  `in-coercer` internally. A coercer for the `email` symbol (using
  `:confirmations/email` dispatch value) was not recognized at compile-time so
  the call to `io.randomseed.utils.db/coerce-in*` was generated instead.

  Let's have a quick look at some real-world example:

  ```
  (io.randomseed.utils.db/<<-  [:confirmations id code token reason id-type expires id id])
  ```

  And generated Clojure code (phase 1):

  ```
  (let [DB__confirmations_id_id_54377141 (io.randomseed.utils.db/<- :confirmations/id id)]
    [DB__confirmations_id_id_54377141
     (io.randomseed.utils.db/<- :confirmations/code       code)
     (io.randomseed.utils.db/<- :confirmations/token     token)
     (io.randomseed.utils.db/<- :confirmations/reason   reason)
     (io.randomseed.utils.db/<- :confirmations/id-type id-type)
     (io.randomseed.utils.db/<- :confirmations/expires expires)
     DB__confirmations_id_id_54377141
     DB__confirmations_id_id_54377141])
  ```

  And fully expanded:

  ```
  (let* [DB__confirmations_id_id_54377141 (#<Fn@5a424a5a ferment.identity/__GT_db> id)]
    [DB__confirmations_id_id_54377141
     (#<Fn@279d4dd9 io.randomseed.utils/safe_parse_long>       code)
     (#<Fn@7f1abd95 io.randomseed.utils/some_str>             token)
     (#<Fn@7f1abd95 io.randomseed.utils/some_str>            reason)
     (#<Fn@7f1abd95 io.randomseed.utils/some_str>           id-type)
     (#<Fn@4ac5d426 ferment.model.confirmation/to_expiry> expires)
     DB__confirmations_id_id_54377141
     DB__confirmations_id_id_54377141])
  ```

  A SQL query which uses the sequence of values presented above needs one of
  them (identified with the `id`) to be repeated. We can observe that the macro
  generated `let` binding for it to assign the result of calling
  `ferment.identity/->db` on `id` to auto-generated symbol named
  `DB__confirmations_id_id_54377141`. This symbol is then re-used in output vector
  multiple times so the calculation is performed just once.

  Also, other coercers were successfully resolved to function objects during macro
  expansion since we have static table and column specifiers given.

  Rule of a thumb is: if you can express certain values or specifications with
  literal strings or keywords, it may speed things up."
  [& specs]
  (let [pre-converted (pp-conv-specs specs)
        bindings      (repeating-qslot-bindings pre-converted)]
    (->> (replace-bindable bindings pre-converted)
         (mapcat parse-conv-spec) vec
         (prepend-qslot-bindings bindings))))

(defmacro <<-*
  "Same as `<<-` but its last argument should be a sequence to be concatenated with the
  results without any pre-processing."
  {:see-also ["<<-"]}
  ([spec]
   `(<<- ~spec))
  ([spec & specs]
   `(into (or (<<- ~@(cons spec (butlast specs))) []) ~(last specs))))

(defn simple->
  [table m]
  (let [table (u/some-str table)]
    (map/map-vals-by-kv #(-> (keyword table (name %1)) %2) m)))

(defn map->
  ([m]
   (map/map-vals-by-kv -> m))
  ([table m]
   (let [table (u/some-str table)]
     (map/map-vals-by-kv
      #(-> (if (namespace %1) %1 (keyword table (name %1))) %2)
      m))))

(defmacro defcoercions
  "Defines input and output coercions for a database table `table`. The `specs` should
  be an argument list consisting of triples in a form of `column-name`,
  `input-coercer`, `output-coercer`.

  For each definition 4 multimethod implementations will be emitted, identified by a
  keyword having a namespace the same as the given table, and a name the same as
  currently processed column name, both in 2 variants: one for snake, and one for
  lisp case. Two multimethod definitions will be created for
  `io.randomseed.utils.db/in-coercer` and two for `io.randomseed.utils.db/out-coercer`.

  If the given coercer is a literal `nil` or `false` value, it will be marked as
  undefined using `false` value. It is advised to use that approach instead of
  assigning `identity` function to express that coercion is not needed since it can
  cause compile-time calculations to short-circuit instead of generating fallback
  code.

  Example:

  `(defcoercions :users :some-identifier str keyword)`

  The above will expand the following code:

  ```
  (defmethod io.randomseed.utils.db/in-coercer  :users/some-identifier [_] str)
  (defmethod io.randomseed.utils.db/in-coercer  :users/some_identifier [_] str)
  (defmethod io.randomseed.utils.db/out-coercer :users/some-identifier [_] keyword)
  (defmethod io.randomseed.utils.db/out-coercer :users/some_identifier [_] keyword)
  ```

  This will allow specialized database coercion functions to transformed values which
  are exchanged with a database.

  Optionally coercions can be defined without a table name. In such case the table
  should be set to either `nil`, `false` or `:io.randomseed.utils.db/any`."
  [table & specs]
  (let [t# (if (identical? table ::any) nil (u/some-str table))]
    `(do
       ~@(mapcat
          (fn [[c# in# out#]]
            `((defmethod  in-coercer ~(sql/colspec-kw    t# c#) [~'_] (or ~in#  false))
              (defmethod  in-coercer ~(sql/make-kw-snake t# c#) [~'_] (or ~in#  false))
              (defmethod out-coercer ~(sql/colspec-kw    t# c#) [~'_] (or ~out# false))
              (defmethod out-coercer ~(sql/make-kw-snake t# c#) [~'_] (or ~out# false))))
          (partition 3 specs))
       nil)))

(defn- get-db-out-coercer
  "Gets database output coercer on a basis of table name and column label from a result
  set metadata object (`rsm`) and index number (`i`). If there is no coercer found
  and column label differs from column name, tries with table and column
  name. Returns a function (coercer found), `false` (coercer found but value should
  remain as-is) or `nil` (coercer not found)."
  [^ResultSetMetaData rsm ^Integer i]
  (let [tab-name  (.getTableName   rsm i)
        col-label (.getColumnLabel rsm i)]
    (if-some [coercer-fn (get-out-coercer* (sql/make-kw-simple tab-name col-label))]
      coercer-fn
      (let [col-name (.getColumnName rsm i)]
        (when (not= col-label col-name)
          (get-out-coercer* (sql/make-kw-simple tab-name col-name)))))))

(defn- delayed-column-by-index-fn
  "Adds coercion to a database result set `rs` handled by builder `builder` with result
  index `i`. For each result it reads its table name and column label (or name, if
  label is not set or is different from label but no coercer has been found), and
  calls output coercer obtained using `io.randomseed.utils.db/out-coercer`. Each result is
  wrapped in a Delay object unless it does not require coercion."
  [_builder ^ResultSet rs ^Integer i]
  (let [^ResultSetMetaData rsm (.getMetaData rs)
        v                      (.getObject rs i)
        coercer-fn             (get-db-out-coercer rsm i)]
    (rs/read-column-by-index (if coercer-fn (delay (coercer-fn v)) v) rsm i)))

(defn- column-by-index-fn
  "Adds coercion to a database result set `rs` handled by builder `builder` with result
  index `i`. For each result it reads its table name and column label (or name, if
  label is not set or is different from label but no coercer has been found), and
  calls output coercer using `io.randomseed.utils.db/->` passing to it the mentioned names and
  a value."
  [_builder ^ResultSet rs ^Integer i]
  (let [^ResultSetMetaData rsm (.getMetaData rs)
        v                      (.getObject rs i)
        coercer-fn             (get-db-out-coercer rsm i)]
    (rs/read-column-by-index (if coercer-fn (coercer-fn v) v) rsm i)))

(defn gen-builder
  "Generates result set builder on a basis of the given builder `rs-builder`. Uses
  `io.randomseed.utils.db/column-by-index-fn` to coerce the results."
  [rs-builder]
  (rs/builder-adapter rs-builder column-by-index-fn))

(defn gen-builder-delayed
  "Generates result set builder on a basis of the given builder `rs-builder`. Uses
  `io.randomseed.utils.db/delayed-column-by-index-fn` to coerce the results."
  [rs-builder]
  (rs/builder-adapter rs-builder delayed-column-by-index-fn))

;; Predefined database options

(def opts-map               (update db/opts-map         :builder-fn gen-builder))
(def opts-simple-map        (update db/opts-simple-map  :builder-fn gen-builder))
(def opts-vec               (update db/opts-vec         :builder-fn gen-builder))
(def opts-simple-vec        (update db/opts-simple-vec  :builder-fn gen-builder))
(def opts-slashed-map       (update db/opts-slashed-map :builder-fn gen-builder))
(def opts-slashed-vec       (update db/opts-slashed-vec :builder-fn gen-builder))
(def opts-lazy-vec          (update db/opts-vec         :builder-fn gen-builder-delayed))
(def opts-lazy-simple-vec   (update db/opts-simple-vec  :builder-fn gen-builder-delayed))
(def opts-lazy-slashed-vec  (update db/opts-slashed-vec :builder-fn gen-builder-delayed))
(def opts-lazy-map          (update db/opts-map         :builder-fn gen-builder-delayed))
(def opts-lazy-simple-map   (update db/opts-simple-map  :builder-fn gen-builder-delayed))
(def opts-lazy-slashed-map  (update db/opts-slashed-map :builder-fn gen-builder-delayed))

;; Query params

(defmacro <q
  "Simple wrapper around `io.randomseed.utils.db.sql/build-query` and `<<-` macros. First
  argument should be a query (possibly grouped with a vector, if multiple arguments
  need to be passed), all other arguments are passed to `<<-`.

  Produces a sequence suitable to be used with `execute-*` family of functions (a
  parameterized query as its first element and coerced query parameters as other
  elements).

  Example:

  ```
  (<q [\"select %(id) from %[u] where %(id) = ? AND %(t) = ?\"
       {:id :users/id, :u :users/id, :t :users/account-type}]
      [:users [:id \"42\" :account-type :user]])
  ```

  The above will return:

  ```
  (\"select `id` from `users` where `id` = ? AND `account_type` = ?\" 42 \"user\")
  ```

  Note that `\"42\"` and `:user` are values which are to be coerced (first with a
  coercer registered for `:users/id` and second with a coercer registered for
  `:users/account-type`). After the coercion resulting values (`42` and `\"user\"`)
  are placed in a sequence to become query parameters."
  [query & params]
  `(cons (sql/build-query ~query) (<<- ~@params)))

(defmacro <dq
  "Simple wrapper around `io.randomseed.utils.db.sql/build-query-dynamic` and `<<-` macros. First
  argument should be a query (possibly grouped with a vector, if multiple arguments
  need to be passed), all other arguments are passed to `<<-`.

  Produces a sequence suitable to be used with `execute-*` family of functions (a
  parameterized query as its first element and coerced query parameters as other
  elements).

  Intended to be used for dynamically generated database queries. Uses a bit slower
  but safer FIFO cache of default size (about 150k items).

  Example:

  ```
  (<q [\"select %(id) from %[u] where %(id) = ? AND %(t) = ?\"
       {:id :users/id, :u :users/id, :t :users/account-type}]
      [:users [:id \"42\"] [:account-type :user]])
  ```

  The above will return:

  ```
  (\"select `id` from `users` where `id` = ? AND `account_type` = ?\" 42 \"user\")
  ```

  Note that `\"42\"` and `:user` are values which are to be coerced (first with a
  coercer registered for `:users/id` and second with a coercer registered for
  `:users/account-type`). After the coercion resulting values (`42` and `\"user\"`)
  are placed in a sequence to become query parameters."
  [query & params]
  `(cons (sql/build-query-dynamic ~query) (<<- ~@params)))

(defmacro <d-do!
  "Calls function `f`, passing `db` as a first argument and a sequence of:
  - a query (being a result of transforming `query` with `io.randomseed.utils.db.sql/build-query-dynamic`),
  - coerced parameters (being a result of transforming `params` with `<<-`)."
  [f db query & params]
  `(~f ~db (cons (sql/build-query-dynamic ~query) (<<- ~@params))))

(defmacro <d-exec!
  "Calls `execute!`, passing `db` as a first argument and a sequence of:
  - a query (being a result of transforming `query` with `io.randomseed.utils.db.sql/build-query-dynamic`),
  - coerced parameters (being a result of transforming `params` with `<<-`)."
  [db query & params]
  `(execute! ~db (cons (sql/build-query-dynamic ~query) (<<- ~@params))))

(defmacro <d-exec-one!
  "Calls `execute-one!`, passing `db` as a first argument and a sequence of:
  - a query (being a result of transforming `query` with `io.randomseed.utils.db.sql/build-query-dynamic`),
  - coerced parameters (being a result of transforming `params` with `<<-`)."
  [db query & params]
  `(execute-one! ~db (cons (sql/build-query-dynamic ~query) (<<- ~@params))))

(defmacro <do!
  "Calls function `f` passing `db` as a first argument and a sequence of:
  - a query (being a result of transforming `query` with `io.randomseed.utils.db.sql/build-query`),
  - coerced parameters (being a result of transforming `params` with `<<-`)."
  [f db query & params]
  `(~f ~db (cons (sql/build-query ~query) (<<- ~@params))))

(defmacro <exec!
  "Calls `execute!`, passing `db` as a first argument and a sequence of:
  - a query (being a result of transforming `query` with `io.randomseed.utils.db.sql/build-query`),
  - coerced parameters (being a result of transforming `params` with `<<-`)."
  [db query & params]
  `(execute! ~db (cons (sql/build-query ~query) (<<- ~@params))))

(defmacro <exec-one!
  "Calls `execute-one!`, passing `db` as a first argument and a sequence of:
  - a query (being a result of transforming `query` with `io.randomseed.utils.db.sql/build-query`),
  - coerced parameters (being a result of transforming `params` with `<<-`)."
  [db query & params]
  `(execute-one! ~db (cons (sql/build-query ~query) (<<- ~@params))))

;; Main wrappers

(defn lazy-execute-one!
  ([connectable sql-params opts]
   (let [m (jdbc/execute-one! connectable sql-params (conj opts-lazy-simple-map opts))]
     (if m (map/to-lazy m) m)))
  ([connectable sql-params]
   (lazy-execute-one! connectable sql-params nil)))

(defn lazy-execute!
  ([connectable sql-params opts]
   (when-some [coll (jdbc/execute! connectable sql-params (conj opts-lazy-simple-map opts))]
     (mapv #(if % (map/to-lazy %) %) coll)))
  ([connectable sql-params]
   (lazy-execute! connectable sql-params nil)))

(defn execute-one!
  ([connectable sql-params opts]
   (jdbc/execute-one! connectable sql-params (conj opts-simple-map opts)))
  ([connectable sql-params]
   (execute-one! connectable sql-params nil)))

(defn execute!
  ([connectable sql-params opts]
   (jdbc/execute! connectable sql-params (conj opts-simple-map opts)))
  ([connectable sql-params]
   (execute! connectable sql-params nil)))

(defmacro lazy-do
  [& cmd]
  `(let [r# ~@cmd] (if r# (map/to-lazy r#) r#)))

(defn lazy-get-by-id
  "Like `next.jdbc/get-by-id` but supports lazy maps."
  ([connectable table pk]
   (lazy-get-by-id connectable table pk :id {}))
  ([connectable table pk opts]
   (lazy-get-by-id connectable table pk :id opts))
  ([connectable table pk pk-name opts]
   (let [opts (conj (or (:options connectable) {}) opts-lazy-simple-map opts)]
     (lazy-do (next-sql/get-by-id connectable table pk pk-name opts)))))

;; Abstract getters and setters

(defn make-getter
  ([f opts id-col cols]
   (make-getter f opts nil id-col cols nil))
  ([f opts table id-col cols]
   (make-getter f opts table id-col cols nil))
  ([f opts table id-col cols getter-coll-fn]
   (let [id-col (keyword id-col)
         cols   (if (map?  cols) (keys cols) cols)
         cols   (if (coll? cols) (seq  cols) cols)
         cols   (if (coll? cols) cols [(or cols "*")])
         table  (sql/to-snake-simple table)
         q      (u/str-spc "SELECT" (sql/join-col-names cols)
                           "FROM"   (or table "?")
                           "WHERE"  (sql/to-snake-simple id-col) "= ?")]
     (if table
       (if getter-coll-fn
         (fn db-lazy-getter
           ([db id]          (db-lazy-getter db nil id))
           ([db _ id]        (f db [q id] opts))
           ([db _ id & more] (getter-coll-fn db (cons id more))))
         (fn [db _ id]
           (f db [q id] opts)))
       (if getter-coll-fn
         (fn
           ([db table id]        (f db [q (sql/to-snake-simple table) id] opts))
           ([db table id & more] (getter-coll-fn db table (cons id more))))
         (fn [db table id]
           (f db [q (sql/to-snake-simple table) id] opts)))))))

(defn make-getter-coll
  "Creates a database getter suitable for use with `get-cached-coll-` family of
  functions. The returned function should accept an argument containing multiple
  identifiers."
  ([f opts id-col]
   (make-getter-coll f opts nil id-col nil))
  ([f opts id-col cols]
   (make-getter-coll f opts nil id-col cols))
  ([f opts table id-col cols]
   (let [id-col (keyword id-col)
         cols   (if (map? cols) (keys cols) cols)
         cols   (if (coll? cols) (seq cols) cols)
         cols   (if (coll? cols) cols [(or cols "*")])
         table  (sql/to-snake-simple table)
         q      (u/str-spc "SELECT" (sql/join-col-names cols)
                           "FROM"   (or table "?")
                           "WHERE"  (sql/to-snake-simple id-col)
                           "IN (")]
     (if table
       (fn db-getter-coll
         ([db ids]   (db-getter-coll db nil ids))
         ([db _ ids] (when-some [ids (seq ids)]
                       (let [query (str q (sql/join-? ids) ")")]
                         (->> (f db (cons query ids) opts)
                              (reduce #(map/qassoc %1 (get %2 id-col) %2) {}))))))
       (fn [db table ids]
         (when-some [ids (seq ids)]
           (let [ids   (map id-to-db ids)
                 table (sql/to-snake-simple table)
                 query (str q (sql/join-? ids) ")")]
             (->> (f db (cons query (cons table ids)) opts)
                  (reduce #(map/qassoc %1 (get %2 id-col) %2) {})))))))))

;; Coercion examples

(comment
  (defn- email-to-db    ^String [v] (identity/->db :email v))
  (defn- phone-to-db    ^String [v] (identity/->db :phone v))
  (defn- long-or-nil    ^Long   [n] (when n (long n)))
  (defn- identity-to-db         [v] (identity/->db v))
  (defn- ip-v6-str      ^String [v] (some-> v ip/to-address ip/to-v6 ip/to-str-v6))

  (defcoercions ::any
    :identity          identity-to-db               identity/of
    :email             email-to-db                  u/some-str
    :phone             phone-to-db                  identity/preparse-phone
    :account-type      u/some-str                   u/some-keyword
    :first-name        u/some-str                   u/some-str
    :middle-name       u/some-str                   u/some-str
    :last-name         u/some-str                   u/some-str
    :ip                ip-v6-str                    ip-v6-str
    :ip-address        ip-v6-str                    ip-v6-str
    :client-ip         ip-v6-str                    ip-v6-str
    :password-suite-id u/safe-parse-long            long-or-nil
    :password          nil                          nil))
