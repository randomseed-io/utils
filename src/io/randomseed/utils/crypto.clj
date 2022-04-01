(ns

    ^{:doc    "Random utilities, crypto support functions."
      :author "Paweł Wilk"
      :added  "1.0.0"}

    io.randomseed.utils.crypto

  (:refer-clojure :exclude [parse-long uuid random-uuid])

  (:import [java.util Random]
           [java.io   Console])

  (:require [crypto.equality     :as       eq]
            [buddy.core.crypto   :as   crypto]
            [buddy.core.codecs   :as   codecs]
            [buddy.core.nonce    :as    nonce]
            [buddy.core.hash     :as     hash]
            [io.randomseed.utils :refer  :all]))

;;
;; Key encryption
;;

(defn salt->bin
  [v]
  (codecs/b64u->bytes (codecs/str->bytes (some-str v))))

(defn key->bin
  [v]
  (codecs/b64u->bytes (codecs/str->bytes (some-str v))))

(defn pwd->bin
  [v]
  (hash/sha256 (some-str v)))

(defn encrypt-key
  "Encrypts private key using random IV and the given password. Returns base64-encoded
  map of two keys."
  [k password]
  (when (and k password)
    (let [p (pwd->bin password)
          n (nonce/random-bytes 16)]
      {:salt (codecs/bytes->str (codecs/bytes->b64u n))
       :key  (codecs/bytes->str (codecs/bytes->b64u
                                 (crypto/encrypt (codecs/to-bytes (str k)) p n
                                                 {:alg :aes128-cbc-hmac-sha256})))})))

(defn decrypt-key-core
  [encrypted k iv]
  (when (and encrypted k iv)
    (try
      (codecs/bytes->str
       (crypto/decrypt encrypted k iv {:alg :aes128-cbc-hmac-sha256}))
      (catch Exception e nil))))

(defn decrypt-key-bin
  [[iv-bin encrypted-bin] pwd-bin]
  (decrypt-key-core encrypted-bin pwd-bin iv-bin))

(defn decrypt-key
  ([m password] (decrypt-key (:key m) (:salt m) password))
  ([encrypted salt password]
   (decrypt-key-bin
    [(salt->bin salt) (key->bin encrypted)]
    (pwd->bin password))))

(defn read-pwd
  ([]
   (read-pwd "Enter password: "))
  ([prompt]
   (when-some [c ^java.io.Console (System/console)]
     (some-str (String/copyValueOf (.readPassword ^java.io.Console c prompt nil))))))

(defn read-key
  ([]
   (read-key "Enter key: "))
  ([prompt]
   (when-some [c ^java.io.Console (System/console)]
     (some-str (.readLine ^java.io.Console c prompt nil)))))
