(ns io.randomseed.utils.var

  ^{:doc    "Random utils, global variables handling."
    :author "Paweł Wilk"
    :added  "1.0.0"}

  (:refer-clojure :exclude [deref resolve reset alter])

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
  [n value]
  (if-some [n (resolve-core (symbol n))]
    (alter-var-root n (constantly value))
    (do (intern (symbol (namespace n)) (symbol (name n)) value)
        value)))
