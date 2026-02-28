(ns

    ^{:doc    "Random Utilities, authentication, appender which always fails."
      :author "Paweł Wilk"
      :added  "2.0.6"}

    io.randomseed.utils.auth.algo.fail

  (:require [io.randomseed.utils.auth.pwd :as         pwd]
            [io.randomseed.utils.map      :as         map]
            [io.randomseed.utils.map      :refer [qassoc]]
            [io.randomseed.utils          :as      utils]))

(defn encrypt
  "Returns password payload with `nil` password (for fail-safe/negative flows)."
  ([plain]
   (encrypt plain {} {}))
  ([plain options]
   (encrypt plain options {}))
  ([plain options settings]
   (if (map? options)
     (qassoc options :password nil)
     {:password nil})))

(def check (partial pwd/standard-check (constantly nil)))

(def handler
  {:encrypt-fn encrypt
   :check-fn   check})
