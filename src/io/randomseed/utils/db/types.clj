(ns

    ^{:doc    "Opinionated database type conversions."
      :author "Paweł Wilk"
      :added  "1.0.0"}

    io.randomseed.utils.db.types

  (:require [potemkin.namespaces  :as  p]
            [next.jdbc.result-set :as rs]
            [next.jdbc.prepare    :as jp])

  (:import  [java.sql Blob Connection PreparedStatement Timestamp]
            [javax.sql DataSource]
            [java.lang.reflect Method]
            [java.util Calendar GregorianCalendar TimeZone]
            [java.io Closeable]
            [inet.ipaddr.ipv4 IPv4Address]
            [inet.ipaddr.ipv6 IPv6Address]))

(set! *warn-on-reflection* true)

(defonce ^TimeZone utc-time-zone
  (TimeZone/getTimeZone "UTC"))

;; Type conversions when reading from a DB

(defonce add-reader-date
  (fn []
    (extend-protocol rs/ReadableColumn

      java.sql.Date

      (read-column-by-label [^java.sql.Date v _]     ^java.sql.Date v)
      (read-column-by-index [^java.sql.Date v _2 _3] ^java.sql.Date v)

      java.sql.Timestamp

      (read-column-by-label [^Timestamp v _]     (.toInstant ^Timestamp v))
      (read-column-by-index [^Timestamp v _2 _3] (.toInstant ^Timestamp v)))))

(defonce add-reader-blob
  (fn []
    (extend-protocol rs/ReadableColumn

      java.sql.Blob

      (read-column-by-label [^Blob v _]     (.getBytes ^Blob v (long 1) ^long (.length ^Blob v)))
      (read-column-by-index [^Blob v _2 _3] (.getBytes ^Blob v (long 1) ^long (.length ^Blob v))))))

(defn add-all-readers
  []
  (add-reader-date)
  (add-reader-blob))

;; Type conversions when writing to a DB

(defonce add-setter-date
  (fn []
    (extend-protocol jp/SettableParameter

      java.sql.Date

      (set-parameter [^java.sql.Date v ^PreparedStatement ps ^long i]
        (.setDate ^PreparedStatement ps ^long i ^java.sql.Date v))

      java.sql.Timestamp

      (set-parameter [^Timestamp v ^PreparedStatement ps ^long i]
        (.setTimestamp ^PreparedStatement ps ^long i ^java.sql.Timestamp v))

      java.time.Instant

      (set-parameter [^java.time.Instant v ^PreparedStatement ps ^long i]
        (.setTimestamp ^PreparedStatement ps ^long i
                       ^Timestamp (Timestamp/from ^java.time.Instant v)
                       ^Calendar  (Calendar/getInstance ^TimeZone utc-time-zone)))

      java.time.ZonedDateTime

      (set-parameter [^java.time.LocalDate v ^PreparedStatement ps ^long i]
        (.setTimestamp ^PreparedStatement ps ^long i
                       ^Timestamp (Timestamp/from ^java.time.Instant (.toInstant ^java.time.ZonedDateTime v))
                       ^Calendar  (Calendar/getInstance ^TimeZone utc-time-zone)))

      java.time.LocalDate

      (set-parameter [^java.time.LocalDate v ^PreparedStatement ps ^long i]
        (.setTimestamp ^PreparedStatement ps ^long i
                       ^Timestamp (Timestamp/valueOf ^java.time.LocalDateTime (.atStartOfDay ^java.time.LocalDate v))
                       ^Calendar  (Calendar/getInstance)))

      java.time.LocalDateTime

      (set-parameter [^java.time.LocalDateTime v ^PreparedStatement ps ^long i]
        (.setTimestamp ^PreparedStatement ps ^long i
                       ^Timestamp (Timestamp/valueOf ^java.time.LocalDateTime v)
                       ^Calendar  (Calendar/getInstance)))

      java.util.Date

      (set-parameter [^java.util.Date v ^PreparedStatement ps ^long i]
        (.setTimestamp ^PreparedStatement ps ^long i
                       ^Timestamp (Timestamp/from ^java.time.Instant (.toInstant ^java.util.Date v))
                       ^Calendar  (Calendar/getInstance ^TimeZone utc-time-zone))))))

(defonce add-setter-ip-address
  (fn []
    (extend-protocol jp/SettableParameter

      IPv4Address

      (set-parameter [^IPv4Address v ^PreparedStatement ps ^long i]
        (.setBytes ^PreparedStatement ps ^long i (.getBytes ^IPv6Address (.toIPv6 ^IPv4Address v))))

      IPv6Address

      (set-parameter [^IPv6Address v ^PreparedStatement ps ^long i]
        (.setBytes ^PreparedStatement ps ^long i (.getBytes ^IPv6Address v))))))

(defn add-all-setters
  []
  (add-setter-date)
  (add-setter-ip-address))

(defn add-all-accessors
  []
  (add-all-readers)
  (add-all-setters))

;; Exposed aliases for builder and conversion functions

(p/import-vars [io.randomseed.utils.db
                to-lisp-simple to-snake-simple to-lisp to-snake
                to-lisp-slashed to-snake-slashed
                opts-simple-map opts-map opts-simple-vec opts-vec
                opts-slashed-map opts-slashed-vec])
