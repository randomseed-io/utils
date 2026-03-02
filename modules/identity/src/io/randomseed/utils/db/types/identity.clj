(ns

    ^{:doc    "Optional DB adapters for identity types."
      :author "Paweł Wilk"
      :added  "2.0.9"}

    io.randomseed.utils.db.types.identity

  (:require [next.jdbc.prepare            :as jp]
            [io.randomseed.utils.identity :as id])

  (:import  (java.sql PreparedStatement)
            (io.randomseed.utils.identity.types Identity)))

(set! *warn-on-reflection* true)

(defonce
  ^{:arglists '([])
    :doc      "Extends `next.jdbc.prepare/SettableParameter` protocol to support
  user identity conversions so `io.randomseed.utils.identity.types.Identity`
  is saved as string."}
  add-setter-identity
  (fn []
    (extend-protocol jp/SettableParameter

      Identity

      (set-parameter [^Identity v ^PreparedStatement ps ^long i]
        (jp/set-parameter (id/to-db* v) ps i)))))

(defn add-all-identity-accessors
  "Adds all identity-specific DB adapters."
  []
  (add-setter-identity))
