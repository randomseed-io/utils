(ns

    ^{:doc    "Random utils, IP address support functions."
      :author "Paweł Wilk"
      :added  "1.0.0"}

    io.randomseed.utils.ip

  (:import  [java.net InetAddress Inet4Address Inet6Address]
            [inet.ipaddr IPAddress IPAddressString IPAddressNetwork]
            [inet.ipaddr.ipv4 IPv4Address IPv4AddressTrie]
            [inet.ipaddr.ipv6 IPv6Address IPv6AddressTrie]))

(defn inet-address?
  "Returns `true` if the given value is of a type `java.net.InetAddress`."
  [v]
  (instance? InetAddress v))

(defn ubyte
  "Converts to an unchecked byte used to express unsigned values. Throws an
  `IllegalArgumentException` exception if the given value is not a byte but exceeds
  the range of 0-255."
  ^Byte [v]
  (if (instance? Byte v) v
      (if (<= 0 v 255)
        (unchecked-byte v)
        (throw (IllegalArgumentException.
                (str "Value out of range for unsigned byte: " v))))))

(defn bytes-to-ipv4
  "Creates new IPv4 address using an array of bytes. When 4 arguments are given, they
  will be used to create a byte array. Will throw an exception if a number given for
  a byte is lesser than 0 or greater than 255."
  ([b]
   (IPv4Address. ^"[B" (bytes b)))
  ([a b c d]
   (->> [a b c d]
        (map ubyte)
        byte-array
        bytes-to-ipv4)))

(defn bytes-to-ipv6
  "Creates new IPv6 address using an array of bytes. When 16 arguments are given, they
  will be used to create a byte array. Will throw an exception if a number given for
  a byte is lesser than 0 or greater than 255."
  ([b]
   (IPv6Address. ^"[B" (bytes b)))
  ([a b c d e f g h i j k l m n o p]
   (->> [a b c d e f g h i j k l m n o p]
        (map ubyte)
        byte-array
        bytes-to-ipv6)))

(defn bytes-to-address
  "Coverts a byte array to an IP address. If there are 4 bytes are less, an IPv4
  address will be created, otherwise IPv6. When more than one argument is given, it
  will create a byte array, converting each argument's value to an unchecked
  byte. Will throw an exception if a number given for a byte is lesser than 0 or
  greater than 255."
  ([b]
   (if (< (count b) 5)
     (IPv4Address. ^"[B" (bytes b))
     (IPv6Address. ^"[B" (bytes b))))
  ([b & bytes]
   (->> (cons b bytes)
        (map ubyte)
        byte-array
        bytes-to-address)))

(defn string-to-address
  "Coverts a string to an IP address."
  [s]
  (when-some [s (and s (not-empty (if (string? s) s (str s))))]
         (let [^IPAddressString ipa (IPAddressString. ^String s)]
           (.getAddress ^IPAddressString ipa))))

(defn number-to-ipv4
  "Coverts a number to an IPv4 address."
  [n]
  (IPv4Address. (int n)))

(defn number-to-ipv6
  "Coverts a number to an IPv6 address."
  [n]
  (IPv6Address. ^java.math.BigInteger (biginteger n)))

(defn number-to-address
  "Coverts a number to an IP address. If the number is a kind of `java.math.BigInteger`
  or `clojure.lang.BigInt`, the IPv6 address will be created. Otherwise the number
  will be converted to `int` and IPv4 address will be created."
  [n]
  (if (or (instance? java.math.BigInteger n)
          (instance? clojure.lang.BigInt  n))
    (IPv6Address. ^java.math.BigInteger (biginteger n))
    (IPv4Address. (int n))))

(defn java-inet-to-address
  "Converts `java.net.InetAddress` to IP address."
  [v]
  (cond
    (instance? Inet6Address v) (bytes-to-ipv6    ^"[B" (.getAddress ^Inet6Address v))
    (instance? Inet4Address v) (bytes-to-ipv4    ^"[B" (.getAddress ^Inet4Address v))
    :else                      (bytes-to-address ^"[B" (.getAddress ^InetAddress  v))))

(defn to-address
  "Converts the given value to an IP address. Can work with strings, byte arrays,
  sequences of bytes and numbers. When 4 (for IPv4) or 16 (for IPv6) arguments are
  given, they should be a bytes or values which may be converted to (unchecked)
  bytes."
  ([s]
   (cond (instance? IPAddress s) s
         (string?       s)       (string-to-address      s)
         (bytes?        s)       (bytes-to-address       s)
         (number?       s)       (number-to-address      s)
         (sequential?   s)       (apply bytes-to-address s)
         (inet-address? s)       (java-inet-to-address   s)
         :else                   (string-to-address      s)))
  ([a b c d]
   (bytes-to-ipv4 a b c d))
  ([a b c d e f g h i j k l m n o p]
   (bytes-to-ipv6 a b c d e f g h i j k l m n o p)))

(def ^{:arglists '([s]
                   [a b c d]
                   [a b c d e f g h i j k l m n o p])}
  address
  "Converts the given value to an IP address. Can work with strings, byte arrays,
  sequences of bytes and numbers. When 4 (for IPv4) or 16 (for IPv6) arguments are
  given, they should be a bytes or values which may be converted to (unchecked)
  bytes."
  to-address)

(defn to-str
  "Converts IP address to a string. Returns `nil` if it is not possible."
  [ip]
  (not-empty (str ip)))

(def ^{:arglists '([ip])}
  to-string
  "Converts IP address to a string. Returns `nil` if it is not possible."
  to-str)

(defn is-ip?
  "Returns `true` if the given value is of type `inet.ipaddr.IPAddress`."
  [v]
  (instance? IPAddress v))

(defn is-ipv4?
  "Returns `true` if the given value is of type `inet.ipaddr.ipv4.IPv4Address`."
  [v]
  (instance? IPv4Address v))

(defn is-ipv6?
  "Returns `true` if the given value is of type `inet.ipaddr.ipv6.IPv6Address`."
  [v]
  (instance? IPv6Address v))

(defn is-ipv4-mapped?
  "Returns `true` if the given value is of type `inet.ipaddr.ipv6.IPv6Address` and
  it is IPv4-mapped address."
  [v]
  (and (instance? IPv6Address v) (.isIPv4Mapped ^IPv6Address v)))

(defn to-v4
  "Converts the given IP address to IPv4. Returns `nil` if this is not possible."
  [^IPAddress v]
  (when v (.toIPv4 ^IPAddress v)))

(defn to-v6
  "Converts the given IP address to IPv6. Returns `nil` if this is not possible."
  [^IPAddress v]
  (when v (.toIPv6 ^IPAddress v)))

(defn ipv6?
  "Returns `true` if the given value (a string, a number, a sequence of bytes, or an IP
  address object) represents a valid IPv6 address."
  [v]
  (boolean
   (when v
     (or (instance? IPv6Address v)
         (and (string? v) (let [^IPAddressString ipa (IPAddressString. ^String v)]
                            (.isIPv6 ^IPAddressString ipa)))
         (instance? IPv6Address (try (to-address v) (catch Exception _ nil)))))))

(defn ipv4?
  "Returns `true` if the given value (a string, a number, a sequence of bytes, or an IP
  address object) represents a valid IPv4 address."
  [v]
  (boolean
   (when v
     (or (instance? IPv4Address v)
         (and (string? v) (let [^IPAddressString ipa (IPAddressString. ^String v)]
                            (.isIPv4 ^IPAddressString ipa)))
         (instance? IPv4Address (try (to-address v) (catch Exception _ nil)))))))

(defn ipv4-mapped?
  "Returns `true` if the given value (a string, a number, a sequence of bytes, or an IP
  address object) represents a valid IPv4 address and it is a IPv4-mapped IPv6 address."
  [v]
  (boolean
   (when v
     (or (and (instance? IPAddress v) (.isIPv4Mapped ^IPv6Address v))
         (and (string? v) (let [^IPAddressString ipa (IPAddressString. ^String v)]
                            (and (.isIPv6 ^IPAddressString ipa)
                                 (.isIPv4Mapped ^IPv6Address (.getAddress ^IPAddressString ipa)))))
         (when-some [v (try (to-address v) (catch Exception _ nil))]
           (and (instance? IPv6Address v) (.isIPv4Mapped ^IPv6Address v)))))))

(defn ip?
  "Returns `true` if the given value (a string, a number, a sequence of bytes, or an IP
  address object) represents a valid IP address."
  [v]
  (boolean
   (when v
     (or (instance? IPAddress v)
         (and (string? v) (.isValid ^IPAddressString (IPAddressString. ^String v)))
         (instance? IPAddress (try (to-address v) (catch Exception _ nil)))))))

(defn in6t?
  "Returns `true` if an IPv6 address is contained within the given tree."
  [trie ^IPAddress ip]
  (and (ipv6? ip)
       (.elementContains ^IPv6AddressTrie trie ^IPv6Address ip)))

(defn in4t?
  "Returns `true` if an IPv6 address is contained within the given tree."
  [trie ^IPAddress ip]
  (and (ipv4? ip)
       (.elementContains ^IPv4AddressTrie trie ^IPv4Address ip)))

(defn to-str-v6
  "Converts IP address to a string expressing IPv6 notation. Returns `nil` if this is
  not possible."
  [ip]
  (not-empty (str (to-v6 ip))))

(def ^{:arglists '([ip])}
  to-string-v6
  "Converts IP address to a string expressing IPv6 notation. Returns `nil` if this is
  not possible."
  to-str-v6)

(defn to-str-v4
  "Converts IP address to a string expressing IPv4 notation. Returns `nil` if this is
  not possible."
  [ip]
  (not-empty (str (to-v4 ip))))

(def ^{:arglists '([ip])}
  to-string-v4
  "Converts IP address to a string expressing IPv4 notation. Returns `nil` if this is
  not possible."
  to-str-v4)

(defn plain-ip
  "Returns an IP address. If the address can be expressed as IPv4, it is returned as
  IPv4, otherwise as IPv6."
  [ip]
  (when ip
    (let [ip (to-address ip)]
      (or (to-v4 ip) ip))))

(defn plain-ip-str
  "Returns string representation of IP address (if the address can be expressed as
  IPv4, it is returned as 4 octets in dotted-decimal notation, otherwise IPv6
  representation is returned)."
  [ip]
  (to-str (plain-ip ip)))

(def ^{:arglists '([ip])}
  to-str-plain
  "Returns string representation of IP address (if the address can be expressed as
  IPv4, it is returned as 4 octets in dotted-decimal notation, otherwise IPv6
  representation is returned)."
  plain-ip-str)

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
                                                (.add ^IPv6AddressTrie t6 ^IPv6Address ip)))))
          (when p4 (locking t4 (doseq [ip p4] (when-not (in4t? t4 ip)
                                                (.add ^IPv4AddressTrie t4 ^IPv4Address ip)))))
          {:ipv4 t4 :ipv6 t6})))))
