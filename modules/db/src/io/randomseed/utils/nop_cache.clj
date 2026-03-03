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
  "Creates a `NOPCache` instance. All lookups return `not-found`, all mutations return
  the cache unchanged. Useful as a no-op placeholder when caching is disabled."
  [& args]
  (NOPCache. (first args)))
