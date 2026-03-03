(ns

    ^{:doc    "Random Utilities, authentication, scrypt algorithm."
      :author "Paweł Wilk"
      :added  "2.0.6"}

    io.randomseed.utils.auth.algo.scrypt

  (:import (com.lambdaworks.crypto SCrypt))

  (:require [io.randomseed.utils.auth.pwd :as         pwd]
            [io.randomseed.utils.map      :as         map]
            [io.randomseed.utils.map      :refer [qassoc]]
            [io.randomseed.utils          :as      utils]))

(def ^{:const true :doc "Default scrypt options: N=32768, r=8, p=1."}
  default-options
  {:cpu-cost 32768
   :mem-cost     8
   :parallel     1})

(def ^{:const true :doc "Keys selected from options/settings for the scrypt operation."}
  required-keys
  [:salt :cpu-cost :mem-cost :parallel])

(defn encrypt
  "Encrypt a password string using the scrypt algorithm."
  {:arglists '([plain]
               [plain options]
               [plain salt]
               [plain options settings]
               [plain salt settings])}
  ([plain]
   (encrypt plain {} {}))
  ([plain options]
   (encrypt plain options {}))
  ([plain options settings]
   (let [options    (if (or (nil? options) (map? options)) options {:salt options})
         options    (conj default-options
                          (map/remove-empty-values (select-keys settings required-keys))
                          (map/remove-empty-values (select-keys options  required-keys)))
         ^"[B" salt (utils/to-bytes (or (get options :salt) (pwd/salt-bytes 16)))
         result     (SCrypt/scrypt
                     ^"[B" (utils/text-to-bytes plain)
                     salt
                     (int (get options :cpu-cost))
                     (int (get options :mem-cost))
                     (int (get options :parallel))
                     (int 32))]
     (qassoc options :salt salt :password result))))

(def ^{:doc "Checker function for the scrypt algorithm (partial of `standard-check`)."}
  check (partial pwd/standard-check encrypt))

(def ^{:doc "Handler map for the scrypt algorithm: encryption/check functions, defaults,
  and shared key list."}
  handler
  {:encrypt-fn encrypt
   :check-fn   check
   :defaults   default-options
   :shared     [:cpu-cost :mem-cost :parallel]})
