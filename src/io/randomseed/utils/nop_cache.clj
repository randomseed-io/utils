(ns

    ^{:doc    "Random utils, NOP-cache."
      :author "Paweł Wilk"
      :added  "1.0.0"}

    io.randomseed.utils.nop-cache

  (:require [clojure.core.cache :refer :all]))

(defcache NOPCache [cache]

  CacheProtocol

  (lookup [_ item]           nil)
  (lookup [_ item not-found] not-found)
  (has?   [_ item]           false)
  (hit    [this item]        this)
  (miss   [this item result] this)
  (evict  [this key]         this)
  (seed   [this base]        this)

  Object

  (toString [_] (str "NOP Cache: " cache)))

(defn factory
  [& args]
  (NOPCache. (first args)))
