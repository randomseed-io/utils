(ns

    ^{:doc    "Optional DB adapters for IP address types."
      :author "Paweł Wilk"
      :added  "2.0.9"}

    io.randomseed.utils.db.types.ip

  (:require [next.jdbc.result-set       :as rs]
            [next.jdbc.prepare          :as jp]
            [io.randomseed.utils.ip     :as ip])

  (:import  (java.net                    InetAddress)
            (java.sql                    PreparedStatement)
            (inet.ipaddr                 IPAddress)
            (inet.ipaddr.ipv4            IPv4Address)
            (inet.ipaddr.ipv6            IPv6Address)))

(set! *warn-on-reflection* true)

(defonce
  ^{:arglists '([])
    :doc      "Extends `next.jdbc.result-set/ReadableColumn` protocol to support IP
  conversions for `inet.ipaddr.IPAddress` and `java.net.InetAddress` values."}
  add-reader-ip-address
  (fn []
    (extend-protocol rs/ReadableColumn

      IPAddress

      (read-column-by-label [^IPAddress v _]     ^IPAddress v)
      (read-column-by-index [^IPAddress v _2 _3] ^IPAddress v)

      InetAddress

      (read-column-by-label [^InetAddress v _]     (ip/java-inet-to-address v))
      (read-column-by-index [^InetAddress v _2 _3] (ip/java-inet-to-address v)))))

(defonce
  ^{:arglists '([])
    :doc      "Extends `next.jdbc.prepare/SettableParameter` protocol to support IP
  conversions so `inet.ipaddr.ipv4.IPv4Address` and `inet.ipaddr.ipv6.IPv6Address`
  are saved as bytes via `.setBytes`."}
  add-setter-ip-address
  (fn []
    (extend-protocol jp/SettableParameter

      IPv4Address

      (set-parameter [^IPv4Address v ^PreparedStatement ps ^long i]
        (.setBytes ^PreparedStatement ps i (.getBytes ^IPv6Address (.toIPv6 ^IPv4Address v))))

      IPv6Address

      (set-parameter [^IPv6Address v ^PreparedStatement ps ^long i]
        (.setBytes ^PreparedStatement ps i (.getBytes ^IPv6Address v))))))

(defn add-all-ip-accessors
  "Adds all IP-specific DB adapters."
  []
  (add-reader-ip-address)
  (add-setter-ip-address))
