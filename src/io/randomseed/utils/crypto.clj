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

(defn ask-pass
  "Ask user for a password with confirmation. Repeats until two passwords are the same
  and are not empty. Keyword arguments can be given to configure behavior:
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
  ([& {:keys [prompt
              confirm-prompt
              not-match-msg
              empty-msg
              retries
              allow-empty?
              empty-nil?
              empty-quits?
              empty-quits-nil?
              confirmation?]
       :or   {allow-empty?     false
              empty-nil?       false
              empty-quits?     false
              empty-quits-nil? true
              confirmation?    true
              prompt           "Enter password: "
              confirm-prompt   "Repeat password: "
              not-match-msg    "Passwords do not match."
              empty-msg        "Password is empty."}}]
   (loop [counter (when retries (unchecked-int (if (pos-int? retries) retries 1)))]
     (when-not (and counter (zero? counter))
       (let [p1 (read-pwd prompt)]
         (if (and (nil? p1) empty-quits?)
           (when-not empty-quits-nil? "")
           (let [p2      (if confirmation? (read-pwd confirm-prompt) p1)
                 counter (when counter (unchecked-dec-int counter))]
             (if (and (nil? p2) empty-quits?)
               (when-not empty-quits-nil? "")
               (if (= p1 p2)
                 (or p1 (if allow-empty? (when-not empty-nil? "") (do (println empty-msg) (recur counter))))
                 (do (println not-match-msg) (recur counter)))))))))))
