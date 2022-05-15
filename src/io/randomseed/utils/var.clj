(ns

    ^{:doc    "Random utils, global variables handling."
      :author "Paweł Wilk"
      :added  "1.0.0"}

    io.randomseed.utils.var

  (:refer-clojure :exclude [deref resolve reset alter update
                            parse-long uuid random-uuid])

  (:require [io.randomseed.utils    :refer :all]
            [io.randomseed.utils.fs :as      fs]))

(defn- resolve-core
  [v]
  (fs/with-ns-loading #(if (ident? v) (clojure.core/resolve (symbol %)) %) v))

(defn resolve
  "Takes a symbol and resolves it, trying to load a namespace if a namespace-qualified
  symbol is provided. For vectors it applies this operation to all values and
  produces a new vector."
  [v]
  (if (vector? v)
    (mapv resolve-core v)
    (resolve-core v)))

(defn- deref-core
  [v]
  (let [v (resolve-core v)]
    (if (var? v) (clojure.core/deref v) v)))

;; (defn deref-with-loading
;;   [v] (when (some? v) (deref-or-call (fs/with-ns-loading v))))

(defn deref
  "Takes a symbol and resolves it using resolve and then dereferences it."
  [v]
  (if (vector? v)
    (mapv deref-core v)
    (deref-core v)))

(defn- deref-with-call-core
  [v]
  (let [v (deref-core v)]
    (if (fn? v) (v) v)))

(defn deref-with-call
  "Tries to resolve a symbol and then dereference a Var if the symbol points to a it or
  if it was already a Var. If the Var points to a functions or it is a function
  object already then it will . For a vector it makes a new vector by applying this operation
  to all of its elements."
  [v]
  (if (vector? v)
    (mapv deref-with-call-core v)
    (deref-with-call-core v)))

(defn deref-symbol
  "Takes a symbol and resolves it using resolve and then dereferences it. If the given
  value is not a symbol it is returned."
  [v]
  (if (symbol? v)
    (deref-core v)
    v))

(defmacro reset
  [v val]
  `(alter-var-root (var ~v) (constantly ~val)))

(defmacro alter
  [v f & args]
  `(alter-var-root (var ~v) ~f ~@args))

(defn make
  "Creates a Var identified in a namespace with a fully qualified identifier `n` and
  sets its value to `value`. If the Var does not exist, it will be created. Returns
  the given value used to change the root binding of a Var."
  [n value]
  (if-some [n (resolve-core (symbol n))]
    (alter-var-root n (constantly value))
    (do (intern (symbol (namespace n)) (symbol (name n)) value)
        value)))

(defn update
  "Updates a Var identified in a namespace with a fully qualified identifier `n`, using a
  function `f`. The function will receive current value of the Var and any optional
  arguments passed, and its returned value will be used to alter the root binding. If
  the Var does not exist, it will be created and the function `f` will receive `nil`
  as its first argument. Returns a value returned by calling `f`."
  [n f & args]
  (if-some [n (resolve-core (symbol n))]
    (apply alter-var-root n f args)
    (let [value (apply f nil args)]
      (intern (symbol (namespace n)) (symbol (name n)) value)
      value)))
