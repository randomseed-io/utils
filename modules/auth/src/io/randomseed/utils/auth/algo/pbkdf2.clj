(ns

    ^{:doc    "Random Utilities, authentication, PBKDF2 algorithm."
      :author "Paweł Wilk"
      :added  "2.0.6"}

    io.randomseed.utils.auth.algo.pbkdf2

  (:import (com.lambdaworks.crypto PBKDF))

  (:require [io.randomseed.utils.auth.pwd :as         pwd]
            [io.randomseed.utils.map      :as         map]
            [io.randomseed.utils.map      :refer [qassoc]]
            [io.randomseed.utils          :as      utils]))

(def ^{:const true :doc "Default PBKDF2 options: 100 000 iterations, HmacSHA256 algorithm."}
  default-options
  {:iterations 100000
   :algorithm  "HmacSHA256"})

(def ^{:const true :doc "Keys selected from options/settings for the PBKDF2 operation."}
  required-keys
  [:salt :iterations :algorithm])

(defn encrypt
  "Encrypts a password string using the PBKDF2 algorithm."
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
                          (map/remove-empty-values (select-keys options required-keys)))
         options    (map/update-existing options :algorithm utils/normalize-name)
         ^"[B" salt (utils/to-bytes (map/lazy-get options :salt (pwd/salt-bytes 8)))
         result     (PBKDF/pbkdf2
                     ^String (get options :algorithm)
                     ^"[B" (utils/text-to-bytes plain)
                     salt
                     (int (get options :iterations))
                     (int 160))]
     (qassoc options :salt salt :password result))))

(def ^{:doc "Checker function for the PBKDF2 algorithm (partial of `standard-check`)."}
  check (partial pwd/standard-check encrypt))

(def ^{:doc "Handler map for the PBKDF2 algorithm: encryption/check functions, defaults,
  and shared key list."}
  handler
  {:encrypt-fn encrypt
   :check-fn   check
   :defaults   default-options
   :shared     [:iterations :algorithm]})
