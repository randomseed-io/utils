(ns

    ^{:doc    "Wrappers on codecs from buddy library."
      :author "Paweł Wilk"
      :added  "1.0.0"}

    io.randomseed.utils.crypto.codecs

  (:require [buddy.core.codecs   :as     codecs]
            [io.randomseed.utils :refer [some-str]]))

(defn plaintext->bin
  "Takes an object, converts it to a string (unless it's already a string), and returns
  its byte-array representation."
  {:added "1.0.0"}
  ^bytes [v]
  (codecs/str->bytes (str v)))

(defn bin->plaintext
  "Takes a byte array and returns a string."
  {:added "1.2.39"}
  [^bytes v]
  (codecs/bytes->str v))

(defn plaintext->base64-url-safe
  "Takes an object, converts it to a string (unless it's already a string), and returns
  a string (Base64-encoded, url-safe representation)."
  {:added "1.2.39"}
  [v]
  (codecs/bytes->b64-str
   (codecs/str->bytes (or (some-str v) ""))
   true))

(defn base64-url-safe->plaintext
  "Takes an object, converts it to a string (unless it's already a string) which should
  be a Base64-encoded, url-safe representation of data, and returns a plain-text
  string."
  {:added "1.2.39"}
  [v]
  (codecs/b64->str
   (codecs/str->bytes (or (some-str v) ""))
   true))

(defn plaintext->base64
  "Takes an object, converts it to a string (unless it's already a string), and returns
  a string (Base64-encoded representation, not url-safe)."
  {:added "1.2.39"}
  [v]
  (codecs/bytes->b64-str
   (codecs/str->bytes (or (some-str v) ""))
   false))

(defn base64->plaintext
  "Takes an object, converts it to a string (unless it's already a string) which should
  be a Base64-encoded representation of data, and returns a plain-text string."
  {:added "1.2.39"}
  [v]
  (codecs/b64->str
   (codecs/str->bytes (or (some-str v) ""))
   false))

(defn base64-url-safe->bin
  "Takes an object, converts it to a string (unless it's already a string) which should
  be a Base64-encoded, url-safe representation of data, and returns a decoded byte array."
  {:added "1.2.39"}
  ^bytes [v]
  (codecs/b64->bytes
   (codecs/str->bytes (or (some-str v) ""))
   true))

(defn bin->base64-url-safe
  "Takes a byte array and returns its Base64-encoded, url-safe representation."
  {:added "1.2.39"}
  [^bytes v]
  (codecs/bytes->b64-str v true))

(defn base64->bin
  "Takes an object, converts it to a string (unless it's already a string) which should
  be a Base64-encoded representation of data, and returns a decoded byte array."
  {:added "1.2.39"}
  ^bytes [v]
  (codecs/b64->bytes
   (codecs/str->bytes (or (some-str v) ""))
   false))

(defn bin->base64
  "Takes a byte array and returns its Base64-encoded representation."
  {:added "1.2.39"}
  [^bytes v]
  (codecs/bytes->b64-str v false))

(def b64-to-bytes base64-url-safe->bin)


