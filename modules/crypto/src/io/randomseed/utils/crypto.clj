(ns

    ^{:doc    "Random utilities, crypto support functions."
      :author "Paweł Wilk"
      :added  "1.0.0"}

    io.randomseed.utils.crypto

  (:require [buddy.core.crypto                 :as   crypto]
            [buddy.core.codecs                 :as   codecs]
            [buddy.core.nonce                  :as    nonce]
            [buddy.core.hash                   :as     hash]
            [io.randomseed.utils.crypto.codecs :as       cc]
            [io.randomseed.utils               :as        u]))

;;
;; Key encryption
;;

(defn salt-b64->bin
  "Decodes Base64-encoded, url-safe salt string to bytes. Returns nil on nil input."
  [v]
  (when v (cc/base64-url-safe->bin v)))

(defn salt-bin->b64
  "Encodes binary salt data to a Base64-encoded, url-safe string."
  [v]
  (cc/bin->base64-url-safe v))

(defn key-b64->bin
  "Decodes Base64-encoded, url-safe key string to bytes. Returns nil on nil
  input."
  [v]
  (when v (cc/base64-url-safe->bin v)))

(defn key-bin->b64
  "Encodes binary key data to Base64-encoded, url-safe string."
  [^bytes v]
  (cc/bin->base64-url-safe v))

(defn key-text->bin
  "Decodes text key to bytes."
  ^bytes [v]
  (cc/plaintext->bin v))

(defn pwd->bin
  "Hashes password with SHA-256 (bytes). Returns nil on nil/empty input."
  ^bytes [pwd]
  (when-some [s (u/some-str pwd)]
    (hash/sha256 s)))

(defn encrypt-key
  "Encrypts a private key `k` using random IV and the given password `password`.
  Returns a map of Base64-encoded, url-safe strings: {:salt ..., :key ...}."
  [k password]
  (when-some [k (u/some-str k)]
    (when-some [password (u/some-str password)]
      (let [^bytes p (pwd->bin password)
            ^bytes n (nonce/random-bytes 16)]
        {:salt (salt-bin->b64 n)
         :key  (key-bin->b64 (crypto/encrypt (key-text->bin k) p n
                                             {:alg :aes128-cbc-hmac-sha256}))}))))

(defn decrypt-key-core
  "Decrypts a binary cryptogram `encrypted` using a key `k` and random
  IV (`iv`). Returns a decrypted message as a string. Assumes the following
  algorithmic cipher-suite: `:aes128-cbc-hmac-sha256`. Optional `algo` argument
  allows for custom suite (will be converted to a keyword if it's not).
  In default suite the key needs to have 32 bytes length and the iv 16 bytes length."
  ([encrypted k iv]
   {:pre [(= (count k) 32) (= (count iv) 16)]}
   (decrypt-key-core encrypted k iv :aes128-cbc-hmac-sha256))
  ([encrypted k iv algo]
   (when (and encrypted k iv)
     (try
       (codecs/bytes->str
        (crypto/decrypt encrypted k iv {:alg (u/some-keyword algo)}))
       (catch Exception _e nil)))))

(defn decrypt-key-bin
  "Decrypts a given binary key. Takes a 2-element vector containing an IV and an
  encrypted key (both in a binary form). Takes a password `pwd-bin` as a second
  argument and uses it to decrypt the given key. Calls `decrypt-key-core`
  internally. Returns a string."
  [[iv-bin encrypted-bin] pwd-bin]
  (decrypt-key-core encrypted-bin pwd-bin iv-bin))

(defn decrypt-key
  "Takes a map `m` consisting of `:key` (encrypted key expressed as a Base64-encoded
  string) and `:salt` (expressed as a Base64-encoded string), and a
  password (expressed as a string), and decrypts the given key. The key and salt can
  be given as separate first arguments (`encrypted` and `salt`) instead of a
  map. Returns decrypted, string representation of a key."
  ([m password]
   (decrypt-key (:key m) (:salt m) password))
  ([encrypted salt password]
   (decrypt-key-bin
    [(salt-b64->bin salt) (key-b64->bin encrypted)]
    (pwd->bin password))))

(defn read-pwd
  "Reads password from a console with an optional prompt. Returns a string or nil.
  The default prompt is: \"Enter password: \")."
  ([]
   (read-pwd "Enter password: "))
  ([prompt]
   (when-some [c ^java.io.Console (System/console)]
     (u/some-str (String/copyValueOf (.readPassword ^java.io.Console c prompt nil))))))

(defn read-key
  "Reads a key from a console with an optional prompt. Returns a string or nil.
  The default prompt is: \"Enter key: \")."
  ([]
   (read-key "Enter key: "))
  ([prompt]
   (when-some [c ^java.io.Console (System/console)]
     (u/some-str (.readLine ^java.io.Console c prompt nil)))))

(defn ask-pass
  "Wrapper around `utils/ask` which reads a password and returns it.
  Options may be passed as a single map OR as keyword args:
  `prompt` (message displayed when asking for first password),
  `confirm-prompt` (message displayed when asking for the same password again),
  `not-match-msg` (message displayed when passwords do not match),
  `empty-msg` (message displayed when the entered password is empty),
  `retries` (number of retries before quitting the loop; when set to `nil` or not
  given, it will continue indefinitely),
  `confirmation?` (requires password to be re-entered for confirmation, defaults to `true`),
  `allow-empty?` (allows the entered password to be an empty string; defaults to `false`),
  `empty-nil?` (returns `nil` instead of an empty string when on empty password; defaults to `false`),
  `empty-quits?` (short-circuits on any empty password and returns `nil`; defaults to `false`),
  `empty-quits-nil?` (returns `nil` when quitting on empty password; defaults to `true`).
  Returns the entered password or `nil`."
  [& opts]
  (let [opts (cond
               (and (= 1 (count opts)) (map? (first opts)))
               (first opts)

               (even? (count opts))
               (apply hash-map opts)

               :else
               (throw (ex-info "ask-pass expects a map or even number of key/value arguments"
                               {:opts opts})))
        defaults {:ask-fn         read-pwd
                  :prompt         "Enter password: "
                  :empty-msg      "Password is empty."
                  :confirm-prompt "Repeat password: "
                  :not-match-msg  "Passwords do not match."}]
    (u/mapply u/ask (merge defaults opts))))

(defn ask-key
  "Wrapper around `utils/ask` which reads a private key (as a string) and returns it.
  Options may be passed as a single map OR as keyword args:
  `prompt` (message displayed when asking for first key),
  `confirm-prompt` (message displayed when asking for the same key again),
  `not-match-msg` (message displayed when keys do not match),
  `empty-msg` (message displayed when the entered key is empty),
  `retries` (number of retries before quitting the loop; when set to `nil` or not
  given, it will continue indefinitely),
  `confirmation?` (requires key to be re-entered for confirmation, defaults to `true`),
  `allow-empty?` (allows the entered key to be an empty string; defaults to `false`),
  `empty-nil?` (returns `nil` instead of an empty string when on empty key; defaults to `false`),
  `empty-quits?` (short-circuits on any empty key and returns `nil`; defaults to `false`),
  `empty-quits-nil?` (returns `nil` when quitting on empty key; defaults to `true`).
  Returns the entered key or `nil`."
  [& opts]
  (let [opts (cond
               (and (= 1 (count opts)) (map? (first opts)))
               (first opts)

               (even? (count opts))
               (apply hash-map opts)

               :else
               (throw (ex-info "ask-key expects a map or even number of key/value arguments"
                               {:opts opts})))
        defaults {:ask-fn            read-key
                  :prompt            "Enter key: "
                  :prompt-confirm    "Repeat key: "
                  :empty-msg         "Key is empty."
                  :confirm-empty-msg "Key is empty."
                  :not-match-msg     "Keys do not match."}]
    (u/mapply u/ask (merge defaults opts))))
