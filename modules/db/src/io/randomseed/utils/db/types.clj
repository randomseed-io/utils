(ns

    ^{:doc    "Opinionated database type conversions."
      :author "Paweł Wilk"
      :added  "1.0.0"}

    io.randomseed.utils.db.types

  (:require [io.randomseed.utils.db]
            [potemkin             :as     p]
            [next.jdbc.result-set :as    rs]
            [next.jdbc.prepare    :as    jp]
            [clj-uuid             :as  uuid]
            [phone-number.core    :as phone])

  (:import  (java.sql                     Blob Connection PreparedStatement Timestamp)
            (javax.sql                    DataSource)
            (java.lang.reflect            Method)
            (java.util                    UUID Calendar GregorianCalendar TimeZone)
            (java.io                      Closeable)
            (inet.ipaddr.ipv4             IPv4Address)
            (inet.ipaddr.ipv6             IPv6Address)
            (com.google.i18n.phonenumbers Phonenumber$PhoneNumber)))

(set! *warn-on-reflection* true)

(defonce ^TimeZone utc-time-zone
  (TimeZone/getTimeZone "UTC"))

;; Type conversions when reading from a DB

(defonce
  ^{:arglists '([])
    :doc      "Extends `next.jdbc.result-set/ReadableColumn` protocol to support date/time
  conversions so `java.sql.Date` data are passed as-is and `java.sql.Timestamp`
  data are converted to `java.sql.Timestamp`."}
  add-reader-date
  (fn []
    (extend-protocol rs/ReadableColumn

      java.sql.Date

      (read-column-by-label [^java.sql.Date v _]     ^java.sql.Date v)
      (read-column-by-index [^java.sql.Date v _2 _3] ^java.sql.Date v)

      java.sql.Timestamp

      (read-column-by-label [^Timestamp v _]     (.toInstant ^Timestamp v))
      (read-column-by-index [^Timestamp v _2 _3] (.toInstant ^Timestamp v)))))

(defonce
  ^{:arglists '([])
    :doc      "Extends `next.jdbc.result-set/ReadableColumn` protocol to support binary large
  object (BLOB) conversions, so `java.sql.Blob` data are converted to an array of
  bytes (`bytes[]`)."}
  add-reader-blob
  (fn []
    (extend-protocol rs/ReadableColumn

      java.sql.Blob

      (read-column-by-label [^Blob v _]     (.getBytes ^Blob v (long 1) ^long (.length ^Blob v)))
      (read-column-by-index [^Blob v _2 _3] (.getBytes ^Blob v (long 1) ^long (.length ^Blob v))))))

(defn add-all-readers
  "Adds all opinionated readers by calling `add-reader-date` and `add-reader-blob`."
  []
  (add-reader-date)
  (add-reader-blob))

;; Type conversions when writing to a DB

(defonce
  ^{:arglists '([])
    :doc      "Extends `next.jdbc.prepare/SettableParameter` protocol to support date/time
  conversions so `java.sql.Date` data are saved using `.setDate` method and
  `java.sql.Timestamp`, `java.time.Instant`, `java.time.ZonedTime`,
  `java.time.LocalDate`, `java.time.LocalDateTime` and `java.util.Date` are saved with
  `.setTimestamp`. Also note that `java.time.Instant`, `java.time.ZonedDateTime` and
  `java.util.Date` are explicitly converted to UTC before saving."}
  add-setter-date
  (fn []
    (extend-protocol jp/SettableParameter

      java.sql.Date

      (set-parameter [^java.sql.Date v ^PreparedStatement ps ^long i]
        (.setDate ^PreparedStatement ps i ^java.sql.Date v))

      java.sql.Timestamp

      (set-parameter [^Timestamp v ^PreparedStatement ps ^long i]
        (.setTimestamp ^PreparedStatement ps i ^java.sql.Timestamp v))

      java.time.Instant

      (set-parameter [^java.time.Instant v ^PreparedStatement ps ^long i]
        (.setTimestamp ^PreparedStatement ps i
                       ^Timestamp (Timestamp/from ^java.time.Instant v)
                       ^Calendar  (Calendar/getInstance ^TimeZone utc-time-zone)))

      java.time.ZonedDateTime

      (set-parameter [^java.time.LocalDate v ^PreparedStatement ps ^long i]
        (.setTimestamp ^PreparedStatement ps i
                       ^Timestamp (Timestamp/from ^java.time.Instant (.toInstant ^java.time.ZonedDateTime v))
                       ^Calendar  (Calendar/getInstance ^TimeZone utc-time-zone)))

      java.time.LocalDate

      (set-parameter [^java.time.LocalDate v ^PreparedStatement ps ^long i]
        (.setTimestamp ^PreparedStatement ps i
                       ^Timestamp (Timestamp/valueOf ^java.time.LocalDateTime (.atStartOfDay ^java.time.LocalDate v))
                       ^Calendar  (Calendar/getInstance)))

      java.time.LocalDateTime

      (set-parameter [^java.time.LocalDateTime v ^PreparedStatement ps ^long i]
        (.setTimestamp ^PreparedStatement ps i
                       ^Timestamp (Timestamp/valueOf ^java.time.LocalDateTime v)
                       ^Calendar  (Calendar/getInstance)))

      java.util.Date

      (set-parameter [^java.util.Date v ^PreparedStatement ps ^long i]
        (.setTimestamp ^PreparedStatement ps i
                       ^Timestamp (Timestamp/from ^java.time.Instant (.toInstant ^java.util.Date v))
                       ^Calendar  (Calendar/getInstance ^TimeZone utc-time-zone))))))

(defonce
  ^{:arglists '([])
    :doc      "Extends `next.jdbc.prepare/SettableParameter` protocol to support IP address
  conversions so `inet.ipaddr.ipv4.IPv4Address` data are converted to `inet.ipaddr.ipv6.IPv6Address`,
  then to an array of bytes and then saved using `.setBytes` method. Similarly,
  `inet.ipaddr.ipv6.IPv6Address` data are converted to an array of bytes and then saved with `.setBytes`."}
  add-setter-ip-address
  (fn []
    (extend-protocol jp/SettableParameter

      IPv4Address

      (set-parameter [^IPv4Address v ^PreparedStatement ps ^long i]
        (.setBytes ^PreparedStatement ps i (.getBytes ^IPv6Address (.toIPv6 ^IPv4Address v))))

      IPv6Address

      (set-parameter [^IPv6Address v ^PreparedStatement ps ^long i]
        (.setBytes ^PreparedStatement ps i (.getBytes ^IPv6Address v))))))

(defonce
  ^{:arglists '([])
    :doc      "Extends `next.jdbc.prepare/SettableParameter` protocol to support UUID
  conversions so `java.util.UUID` data are converted to an array of bytes and then
  saved using `.setBytes` method."}
  add-setter-uuid
  (fn []
    (extend-protocol jp/SettableParameter

      java.util.UUID

      (set-parameter [^UUID v ^PreparedStatement ps ^long i]
        (.setBytes ^PreparedStatement ps i ^bytes (uuid/to-byte-array ^UUID v))))))

(defonce
  ^{:arglists '([])
    :doc      "Extends `next.jdbc.prepare/SettableParameter` protocol to support phone number
  conversions so `com.google.i18n.phonenumbers/Phonenumber$PhoneNumber` data are converted to
  strings (in E.164 format) and then saved."}
  add-setter-phone-number
  (fn []
    (extend-protocol jp/SettableParameter

      Phonenumber$PhoneNumber

      (set-parameter [^Phonenumber$PhoneNumber v ^PreparedStatement ps ^long i]
        (.setString ^PreparedStatement ps i ^String (phone/format v nil :phone-number.format/e164))))))

(defn add-all-setters
  "Adds all opinionated setters by calling `add-setter-date` and `add-setter-ip-address`."
  []
  (add-setter-date)
  (add-setter-ip-address)
  (add-setter-phone-number)
  (add-setter-uuid))

(defn add-all-accessors
  "Adds all opinionated readers and setters by calling `add-all-readers` and `add-all-setters`."
  []
  (add-all-readers)
  (add-all-setters))

;; Exposed aliases for builder and conversion functions

(p/import-vars [io.randomseed.utils.db
                to-lisp-simple to-snake-simple to-lisp to-snake
                to-lisp-slashed to-snake-slashed
                opts-simple-map opts-map opts-simple-vec opts-vec
                opts-slashed-map opts-slashed-vec])
