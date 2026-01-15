(ns io.randomseed.utils.crypto-test
  (:require
   [clojure.test :refer :all]
   [clojure.test.check.clojure-test :refer [defspec]]
   [clojure.test.check.generators :as gen]
   [clojure.test.check.properties :as prop]
   [buddy.core.hash :as hash]
   [io.randomseed.utils :as u]
   [io.randomseed.utils.crypto :as crypto]
   [io.randomseed.utils.crypto.codecs :as cc])
  (:import
   (java.util Arrays)))

(defn bytes=
  ^Boolean [^bytes a ^bytes b]
  (Arrays/equals a b))

(def ^:private url-safe-b64-re
  ;; Buddy's url-safe base64: A-Z a-z 0-9 _ - plus optional '=' padding.
  #"^[A-Za-z0-9_-]*={0,2}$")

(defn b64->str
  [v]
  (cond
    (string? v) v
    (bytes? v)  (cc/bin->plaintext v)
    :else       (str v)))

(defn url-safe-b64?
  [s]
  (and (string? s) (re-matches url-safe-b64-re s)))

(def gen-not-b64-url-safe
  (gen/fmap
   (fn [[a b c]]
     (str a "+" b "/" c))  ; + and / are forbidden in url-safe strings
   (gen/tuple gen/string gen/string gen/string)))

(def gen-bytes
  "Generator byte-array (0..512 bajtów)."
  (gen/fmap
   (fn [v] (byte-array (map byte v)))
   (gen/vector gen/byte 0 512)))

(def gen-nonempty-bytes
  (gen/fmap
   (fn [v] (byte-array (map byte v)))
   (gen/vector gen/byte 1 512))) ; <= minimum 1

(def gen-salt-sample
  (gen/fmap
   (fn [^bytes bs]
     (let [b64 (crypto/salt-bin->b64 bs)]
       {:bytes bs :b64 b64 :len (alength bs)}))
   gen-nonempty-bytes))

(def gen-key-sample
  (gen/fmap
   (fn [^bytes bs]
     (let [b64 (crypto/key-bin->b64 bs)]
       {:bytes bs :b64 b64 :len (alength bs)}))
   gen-nonempty-bytes))

(def gen-strable
  (gen/one-of
   [gen/string
    gen/large-integer
    gen/boolean
    gen/keyword
    gen/uuid]))

(def gen-nonempty-string
  ;; NOTE: utils/some-str treats "" as empty but DOES NOT trim spaces.
  (gen/such-that (complement empty?) gen/string))

(deftest ask-wrappers
  (testing "ask-pass passes defaults and allows override (both map and kv-args)"
    (let [captured (atom nil)]
      (with-redefs [u/ask (fn [& kvs]
                            (reset! captured (apply hash-map kvs))
                            "sekret")]
        (is (= "sekret" (crypto/ask-pass :prompt "P> " :empty-msg "nope")))
        (is (= "P> " (:prompt @captured)))
        (is (= "nope" (:empty-msg @captured)))
        (is (fn? (:ask-fn @captured)))

        (is (= "sekret" (crypto/ask-pass {:prompt "P2> " :empty-msg "nope2"})))
        (is (= "P2> " (:prompt @captured)))
        (is (= "nope2" (:empty-msg @captured))))))

  (testing "ask-key passes defaults and allows override (both map and kv-args)"
    (let [captured (atom nil)]
      (with-redefs [u/ask (fn [& kvs]
                            (reset! captured (apply hash-map kvs))
                            "k")]
        (is (= "k" (crypto/ask-key :prompt "K> " :prompt-confirm "K2> ")))
        (is (= "K> " (:prompt @captured)))
        (is (= "K2> " (:prompt-confirm @captured)))
        (is (fn? (:ask-fn @captured)))

        (is (= "k" (crypto/ask-key {:prompt "K3> " :prompt-confirm "K4> "})))
        (is (= "K3> " (:prompt @captured)))
        (is (= "K4> " (:prompt-confirm @captured)))))))

(deftest pwd->bin-basics
  (testing "pwd->bin matches buddy.core.hash/sha256"
    (is (bytes= (hash/sha256 "abc") (crypto/pwd->bin "abc")))
    (is (bytes= (hash/sha256 " ") (crypto/pwd->bin " "))))
  (testing "nil/\"\" -> nil"
    (is (nil? (crypto/pwd->bin nil)))
    (is (nil? (crypto/pwd->bin "")))))

(defspec salt-roundtrip-bin<->b64 500
  (prop/for-all [{:keys [^bytes bytes b64]} gen-salt-sample]
                (let [bs2 (crypto/salt-b64->bin b64)]
                  (and (url-safe-b64? b64)
                       (some? bs2)
                       (bytes= bytes bs2)))))

(defspec key-roundtrip-bin<->b64 500
  (prop/for-all [{:keys [^bytes bytes b64]} gen-key-sample]
                (let [bs2 (crypto/key-b64->bin b64)]
                  (and (url-safe-b64? b64)
                       (some? bs2)
                       (bytes= bytes bs2)))))

(defspec salt-vs-key-are-same-codec 500
  (prop/for-all [^bytes bs gen-nonempty-bytes]
                (= (crypto/salt-bin->b64 bs)
                   (crypto/key-bin->b64 bs))))

(defspec key-text->bin-roundtrip-plaintext 500
  (prop/for-all [v gen-strable]
                (= (str v)
                   (cc/bin->plaintext (crypto/key-text->bin v)))))

(deftest b64->bin-nil-contract
  (is (nil? (crypto/salt-b64->bin nil)))
  (is (nil? (crypto/key-b64->bin  nil))))

(defspec bin->b64-returns-string 200
  (prop/for-all
   [^bytes bs gen-nonempty-bytes]
   (and (string? (crypto/salt-bin->b64 bs))
        (string? (crypto/key-bin->b64  bs)))))

(defspec salt-vs-key-same-b64-string 200
  (prop/for-all
   [^bytes bs gen-nonempty-bytes]
   (= (b64->str (crypto/salt-bin->b64 bs))
      (b64->str (crypto/key-bin->b64  bs)))))

(defspec b64-decoders-do-not-accept-non-url-safe 300
  (prop/for-all
   [s gen-not-b64-url-safe]
   (let [salt-res (try (crypto/salt-b64->bin s) (catch Throwable _ ::err))
         key-res  (try (crypto/key-b64->bin  s) (catch Throwable _ ::err))]
     (and (not (bytes? salt-res))
          (not (bytes? key-res))))))

(defspec key-text->bin-is-plaintext->bin 300
  (prop/for-all
   [v gen-strable]
   (bytes= (crypto/key-text->bin v)
           (cc/plaintext->bin v))))

(deftest empty-bytes-encoding
  (is (some? (crypto/salt-bin->b64 (byte-array 0))))
  (is (some? (crypto/key-bin->b64  (byte-array 0)))))

(deftest encrypt-decrypt-roundtrip
  (let [k   "sekret-🔑-123"
        p   "haslo-🧂-456"
        enc (crypto/encrypt-key k p)]
    (is (string? (:salt enc)))
    (is (string? (:key enc)))
    (is (= k (crypto/decrypt-key enc p)))
    (is (nil? (crypto/decrypt-key enc "wrong")))))

(defspec encrypt-decrypt-roundtrip-works 150
  (prop/for-all
   [k gen-nonempty-string
    p gen-nonempty-string]
   (let [enc (crypto/encrypt-key k p)]
     (= (str k) (crypto/decrypt-key enc p)))))

(defspec decrypt-with-wrong-password-yields-nil 150
  (prop/for-all
   [k  gen-nonempty-string
    p1 gen-nonempty-string
    p2 gen-nonempty-string]
   (if (= p1 p2)
     true
     (let [enc (crypto/encrypt-key k p1)]
       (nil? (crypto/decrypt-key enc p2))))))

(defspec encryption-is-randomized 60
  (prop/for-all
   [k gen-nonempty-string
    p gen-nonempty-string]
   (let [e1 (crypto/encrypt-key k p)
         e2 (crypto/encrypt-key k p)]
     ;; extremely unlikely collision, but should not happen in practice
     (and (not= (:key e1) (:key e2))
          (not= (:salt e1) (:salt e2))))))

(deftest decrypt-key-overload
  (testing "3-arity overload works"
    (let [k "k"
          p "p"
          {:keys [salt key]} (crypto/encrypt-key k p)]
      (is (= k (crypto/decrypt-key key salt p))))))
