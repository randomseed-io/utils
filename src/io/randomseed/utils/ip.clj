(ns

    ^{:doc    "Random utils, IP address support functions."
      :author "Paweł Wilk"
      :added  "1.0.0"}

    io.randomseed.utils.ip

  (:refer-clojure :exclude [parse-long uuid random-uuid])

  (:require [clojure.string :as          str]
            [io.randomseed.utils :refer :all])

  (:import  [java.net InetAddress Inet4Address Inet6Address]
            [inet.ipaddr IPAddress IPAddressString IPAddressNetwork]
            [inet.ipaddr.ipv4 IPv4Address IPv4AddressTrie]
            [inet.ipaddr.ipv6 IPv6Address IPv6AddressTrie]))

(defn to-address
  [s]
  (if (instance? IPAddress s) s
      (when-some [s (and s (not-empty (str s)))]
        (let [^IPAddressString ipa (IPAddressString. ^String s)]
          (.getAddress ^IPAddressString ipa)))))

(defn to-str
  [ip]
  (not-empty (str ip)))

(defn ipv6?
  [v]
  (boolean
   (or (instance? IPv6Address v) (instance? Inet6Address v)
       (and (some? v)
            (let [^IPAddressString ipa (IPAddressString. ^String (str v))]
              (.isIPv6 ^IPAddressString ipa))))))

(defn to-v6
  [^IPAddress v]
  (.toIPv6 ^IPAddress v))

(defn to-v4
  [^IPAddress v]
  (.toIPv4 ^IPAddress v))

(defn in6t?
  [trie ^IPAddress ip]
  (and (ipv6? ip)
       (.elementContains ^IPv6AddressTrie trie ^IPv6Address ip)))

(defn in4t?
  [trie ^IPAddress ip]
  (and (not (ipv6? ip))
       (.elementContains ^IPv4AddressTrie trie ^IPv4Address ip)))

(defn to-str-v6
  [ip]
  (not-empty (str (to-v6 ip))))

(defn plain-ip
  [ip]
  (when ip
    (let [ip (to-address ip)]
      (or (to-v4 ip) ip))))

(defn plain-ip-str
  [ip]
  (to-str (plain-ip ip)))

(def to-str-plain plain-ip-str)

(defn preprocess-ip-list
  "Takes a sequence of IP addresses and returns a vector of Trie trees with IP address
  ranges."
  [p]
  (when p
    (let [{a :ipv4 b :ipv6} (when (map? p) p)]
      (if (or (and (or (nil? a) (instance? IPv4AddressTrie a)) (instance? IPv6AddressTrie b))
              (and (nil? b) (instance? IPv4AddressTrie a)))
        p
        (let [p  (if (coll? p) p (cons (to-str p) nil))
              p  (if (map? p) (vals p) p)
              p  (->> (map str p)
                      distinct
                      (filter identity)
                      (map #(IPAddressString. ^String          %))
                      (map #(.getAddress      ^IPAddressString %))
                      (map #(.toIPv6          ^IPAddress       %))
                      (group-by ipv6?))
              p6 (seq (get p true))
              p4 (seq (get p false))
              t6 (when p6 (IPv6AddressTrie.))
              t4 (when p4 (IPv4AddressTrie.))]
          (when p6 (locking t6 (doseq [ip p6] (when-not (in6t? t6 ip)
                                                (.add ^IPv6AddressTrie t6
                                                      ^IPv6Address ip)))))
          (when p4 (locking t4 (doseq [ip p4] (when-not (in4t? t4 ip)
                                                (.add ^IPv4AddressTrie t4
                                                      ^IPv4Address ip)))))
          {:ipv4 t4 :ipv6 t6})))))

