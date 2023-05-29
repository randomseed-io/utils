(ns

    ^{:doc    "Random support functions and macros."
      :author "Paweł Wilk"
      :added  "1.0.0"}

    io.randomseed.utils

  (:refer-clojure :exclude [parse-long uuid random-uuid])

  (:import [java.security SecureRandom]
           [java.time     Instant Duration ZoneRegion]
           [java.util     UUID Random Locale Date Calendar Collection Collections ArrayList]
           [java.nio      ByteBuffer]
           [java.io       Console])

  (:require    [clojure.string               :as      str]
               [clojure.set                  :as      set]
               [clojure.java.io              :as       io]
               [clojure.main                 :as    cmain]
               [camel-snake-kebab.core       :as      csk]
               [crypto.equality              :as       eq]
               [buddy.core.crypto            :as   crypto]
               [buddy.core.codecs            :as   codecs]
               [buddy.core.nonce             :as    nonce]
               [buddy.core.hash              :as     hash]
               [trptr.java-wrapper.locale    :as        l]
               [clojure.spec.alpha           :as        s]
               [tick.core                    :as        t]
               [tick.protocols               :as       tp]
               [clj-http.client              :as     http]))

(s/def ::set set?)
(s/def ::map map?)
(s/def ::number number?)
(s/def ::integer int?)
(s/def ::natural nat-int?)
(s/def ::positive pos-int?)
(s/def ::bytes bytes?)
(s/def ::callable ifn?)
(s/def ::vector vector?)
(s/def ::identifier ident?)
(s/def ::not-empty-string (s/and string? not-empty))
(s/def ::simple-identifier simple-ident?)
(s/def ::not-conflicting-ns (fn [[ns id]] (or (simple-ident? id) (= ns (namespace id)))))

;; Types

(defn ^Boolean atom?      [v] (instance? clojure.lang.Atom v))
(defn ^Boolean instant?   [v] (instance? Instant   v))
(defn ^Boolean exception? [v] (instance? Exception v))
(defn ^Boolean throwable? [v] (instance? Throwable v))

;; Values handling

(defn empty-string?
  "Returns `true` if `s` is an empty string. Will throw an exception when `s` is not a
  string."
  ^Boolean [^String s]
  (.isEmpty ^String s))

(defn not-empty-string?
  "Returns `true` if `s` is not an empty string. Will throw an exception when `s` is
  not a string."
  ^Boolean [^String s]
  (not (.isEmpty ^String s)))

(defn empty-ident?
  "Returns `true` if `v` is an empty identifier."
  [v]
  (and (empty-string? (name v))
       (empty-string? (namespace v))))

(defn not-empty-ident?
  "Returns `true` if `v` is not an empty identifier."
  [v]
  (or (not-empty-string? (name v))
      (not-empty-string? (namespace v))))

(defn not-valuable?
  "Returns `true` if `x` not valuable: is `nil` or empty."
  [x]
  (cond (nil? x)     true
        (string?  x) (empty-string? x)
        (ident?   x) (empty-ident?  x)
        (counted? x) (zero? (count  x))
        (seqable? x) (nil?  (seq    x))
        true         false))

(defn valuable?
  "Returns `true` if `x` valuable: is not `nil` nor empty."
  [x]
  (cond (nil?     x) false
        (string?  x) (not-empty-string? x)
        (ident?   x) (not-empty-ident?  x)
        (counted? x) (pos?  (count      x))
        (seqable? x) (some? (seq        x))
        true         true))

(defmacro when-valuable
  "Evaluates expressions from `more` in an implicit `do` when `v` is not `nil` nor
  empty."
  [v & more]
  `(when (valuable? ~v)
     ~@more))

(defmacro when-not-valuable
  "Evaluates expressions from `more` in an implicit `do` when `v` is `nil` or
  empty."
  [v & more]
  `(when (not-valuable? ~v)
     ~@more))

(defmacro valuable
  [& more]
  (if-some [b (butlast more)]
    `(do ~@b (let [l# ~(last more)] (if (valuable? l#) l#)))
    `(let [l# ~(first more)] (if (valuable? l#) l#))))

(defmacro not-valuable
  [& more]
  (if-some [b (butlast more)]
    `(do ~@b (let [l# ~(last more)] (if (not-valuable? l#) l#)))
    `(let [l# ~(first more)] (if (not-valuable? l#) l#))))

;; Text handling

(defn some-str
  "Converts the given value `v` to a string. Keywords are transformed to strings
  without the `:` prefix. Empty string or `nil` will result in `nil` being returned."
  [v]
  (if (string? v)
    (if (empty-string? v) nil v)
    (if (nil? v)
      nil
      (if (ident? v)
        (let [s (str (symbol v))] (if (empty-string? s) nil s))
        (if-some [s (str v)] (if (empty-string? s) nil s))))))

(defn some-str-up
  "Converts the given value `v` to an uppercase string. Keywords are transformed to
  strings without the `:` prefix. Empty string or `nil` will result in `nil` being
  returned."
  [v]
  (if-some [s (some-str v)]
    (str/upper-case s)))

(defn some-str-down
  "Converts the given value `v` to a lowercase string. Keywords are transformed to
  strings without the `:` prefix. Empty string or `nil` will result in `nil` being
  returned."
  [v]
  (if-some [s (some-str v)]
    (str/lower-case s)))

(defn some-str-simple
  "Converts the given value `v` to a string. If `v` is an identifier only its name part
  is used and namespace is ignored. Keywords are transformed to strings without the
  `:` prefix. Empty string or `nil` will result in `nil` being returned."
  [v]
  (some-str
   (if (ident? v) (name v) v)))

(defn some-str-simple-up
  "Converts the given value `v` to an uppercase string. If `v` is an identifier only
  its name part is used and namespace is ignored. Keywords are transformed to strings
  without the `:` prefix. Empty string or `nil` will result in `nil` being returned."
  [v]
  (some-str-up
   (if (ident? v) (name v) v)))

(defn some-str-simple-down
  "Converts the given value `v` to a lowercase string. If `v` is an identifier only its
  name part is used and namespace is ignored. Keywords are transformed to strings
  without the `:` prefix. Empty string or `nil` will result in `nil` being returned."
  [v]
  (some-str-down
   (if (ident? v) (name v) v)))

(defn str-spc
  "Takes one or more strings or other objects convertable to strings and concatenates
  them with spaces."
  [s & more]
  (if-not more
    (str s)
    (apply str (interpose " " (cons s more)))))

(defn str-squeeze-spc
  "Takes one or more strings or other objects convertable to strings and concatenates
  them with spaces and squeezes spaces in a resulting string which is
  returned."
  [s & more]
  (str/replace (if-not more
                 (str s)
                 (apply str (interpose " " (cons s more))))
               #"\s+" " "))

(defn some-str-spc
  "Takes one or more strings or other objects convertable to strings and concatenates
  them with spaces. Keywords are transformed to strings without the `:` prefix."
  [s & more]
  (if-not more
    (str (some-str s))
    (apply str (interpose " " (filter some? (map some-str (cons s more)))))))

(defn some-str-squeeze-spc
  "Takes one or more strings or other objects convertable to strings and concatenates
  them with spaces and squeezes spaces in a resulting string which is
  returned. Keywords are transformed to strings without the `:` prefix."
  [s & more]
  (str/replace (if-not more
                 (str (some-str s))
                 (apply str (interpose " " (filter some? (map some-str (cons s more))))))
               #"\s+" " "))

(defn some-string
  "Takes a string `s` and returns it unless its value is `nil` or it is an empty
  string. A bit more performant but will throw an exception when `s` is not a
  string nor `nil`."
  ^String [^String s]
  (if (or (nil? s) (empty-string? s)) nil s))

(defn strb
  "Like `clojure.core/str` but faster. Be aware that all arguments except first must be
  of type `String` or `nil`."
  (^String [^Object a]
   (if a (.toString ^Object a) ""))
  (^String [^String a ^String b]
   (.toString ^StringBuilder (.append ^StringBuilder (StringBuilder. (or a "")) (or b ""))))
  (^String [^String a ^String b ^String c]
   (.toString
    ^StringBuilder (doto (StringBuilder. (or a ""))
                     (.append (or b ""))
                     (.append (or c "")))))
  (^String [^String a ^String b ^String c ^String d]
   (.toString
    ^StringBuilder (doto (StringBuilder. (or a ""))
                     (.append (or b ""))
                     (.append (or c ""))
                     (.append (or d "")))))
  (^String [^String a ^String b ^String c ^String d ^String e]
   (.toString
    ^StringBuilder (doto (StringBuilder. (or a ""))
                     (.append (or b ""))
                     (.append (or c ""))
                     (.append (or d ""))
                     (.append (or e "")))))
  (^String [^String a ^String b ^String c ^String d ^String e & more]
   (.toString
    ^StringBuilder (doto (StringBuilder. (or a ""))
                     (.append (or b ""))
                     (.append (or c ""))
                     (.append (or d ""))
                     (.append (or e ""))
                     (.append (apply strb more))))))

(defmacro simpstr
  "Helper macro to assemble results of string-joining macros."
  {:no-doc true}
  ([] "")
  ([a]   (if (string? a) a `(str ~a)))
  ([a b] (if (and (or (nil? a) (string? a)) (or (nil? b) (string? b))) `(strb ~a ~b) `(str ~a ~b)))
  ([a b & more] `(str ~a ~b ~@more)))

(defn ^:no-doc sq-spc
  ^String [^String s]
  (if (and s (not= "" s))
    (str/replace s #"\s{2,}" " ")
    s))

(defn ^:no-doc str-convertable?
  ^Boolean [v]
  (or (string?  v)
      (number?  v)
      (keyword? v)
      (nil?     v)
      (boolean? v)))

(defmacro strs
  "Converts all arguments to strings and joins them. Neighbouring literal strings or
  string-convertable values (literal keywords, numbers, booleans and nil values) will
  be concatenated at compile time."
  ([]
   "")
  ([a]
   (if (str-convertable? a) `~(or (some-str a) "") `(str (some-str ~a) "")))
  ([a & more]
   `(simpstr ~@(->> (cons a more)
                    (partition-by str-convertable?)
                    (mapcat #(if (str-convertable? (first %))
                               (cons (apply strb (map some-str %)) nil)
                               %))
                    (partition-by string?)
                    (mapcat #(if (string? (first %)) (cons (apply strb %) nil) %))))))

(defmacro strspc
  "Converts all arguments to strings and joins them with spaces. Neighbouring literal
  strings or string-convertable values (literal keywords, numbers, booleans and nil
  values) will be concatenated at compile time."
  ([]
   "")
  ([a]
   (if (str-convertable? a) `~(or (some-str a) "") `(str (some-str ~a))))
  ([a & more]
   `(simpstr ~@(->> (cons a more)
                    (partition-by str-convertable?)
                    (mapcat #(if (str-convertable? (first %))
                               (cons (apply strb (interpose " " (map some-str %))) nil)
                               %))
                    (interpose " ")
                    (partition-by string?)
                    (mapcat #(if (string? (first %)) (cons (apply strb %) nil) %))))))

(defmacro strspc-squeezed
  "Converts all arguments to strings and joins them with spaces. Squeezes repeating
  spaces and trims the string. Neighbouring literal strings or string-convertable
  values (literal keywords, numbers, booleans and nil values) will be concatenated,
  squeezed and trimmed at compile time

  If, instead of literal string-convertable value, some other expression will appear,
  squeezing and trimming will NOT be performed on it, nor on its resulting value."
  ([]
   "")
  ([a]
   (if (str-convertable? a) `~(sq-spc (or (some-str a) "")) `(str (some-str ~a) "")))
  ([a & more]
   `(simpstr
     ~@(->> (cons a more)
            (partition-by str-convertable?)
            (mapcat #(if (str-convertable? (first %))
                       (cons (sq-spc (apply strb (interpose " " (map some-str %)))) nil)
                       %))
            (interpose " ")
            (partition-by string?)
            (mapcat #(if (string? (first %)) (cons (apply strb %) nil) %))))))

(defn replace-first
  "Replaces the first appearance of a character `c` in the given string `s` with a
  character `r`."
  [s c r]
  (if (string? s)
    (if-some [idx (str/index-of s c)]
      (let [idx (unchecked-int idx)
            cnt (unchecked-int (count s))]
        (if (> cnt (unchecked-subtract-int cnt idx) 1)
          (str (subs s 0 idx) r (subs s (unchecked-inc-int idx)))
          s))
      s)
    s))

(defn to-lisp-str
  "ip_address --> ip-address"
  [v]
  (if-some [v (some-str v)]
    (csk/->kebab-case-string v)))

(defn to-lisp-simple-str
  "abc/ip_address --> ip-address"
  [v]
  (to-lisp-str (if (ident? v) (name v) v)))

(defn to-lisp-str-replace-first
  "ipCaddress_to --> ipRaddress-to"
  [v c r]
  (if-some [v (some-str v)]
    (csk/->kebab-case-string (replace-first v c r))))

(defn to-lisp-slashed-str
  "ip_address_is --> ip/address-is"
  [v]
  (to-lisp-str-replace-first v \_ \/))

(defn to-snake-str
  "ip-address --> ip_address"
  [v]
  (if-some [v (some-str v)]
    (csk/->snake_case_string v)))

(defn to-snake-simple-str
  "abc/ip-address --> ip_address"
  [v]
  (to-snake-str (if (ident? v) (name v) v)))

(defn to-snake-str-replace-first
  "ipCaddress-to --> ipRaddress_to"
  [v c r]
  (if-some [v (some-str v)]
    (csk/->snake_case_string (replace-first v c r))))

(defn to-snake-slashed-str
  "ip-address-is --> ip/address_is"
  [v]
  (to-snake-str-replace-first v \- \/))

;; Names

(defn normalize-name
  "Takes a name expressed as a string or an identifier. If the object is an identifier
  (a symbol or a keyword) then it converts it to a string using name function. If the
  second argument is present then it uses it when the given name or a name derived
  from identifier is empty."
  ([some-name]
   (normalize-name some-name nil))
  ([some-name default-name]
   (if (ident? some-name)
     (clojure.core/name some-name)
     (if (seqable? some-name)
       (if (seq some-name) (str some-name) default-name)
       (if some-name (str some-name))))))

(defn normalize-name-with-ns
  "Takes a name expressed as a string or an identifier. If the object is an identifier
  (a symbol or a keyword) then it converts it to a string using namespace and name
  functions. If the second argument is present then it uses it when the given name or
  a name derived from identifier is empty."
  ([some-name]
   (normalize-name-with-ns some-name nil))
  ([some-name default-name]
   (if (ident? some-name)
     (if-some [ns-name (clojure.core/namespace some-name)]
       (str ns-name "/" (clojure.core/name some-name))
       (clojure.core/name some-name))
     (if (seqable? some-name)
       (if (seq some-name) (str some-name) default-name)
       (if some-name (str some-name))))))

;; Bytes

(defn b64-to-bytes
  "Converts Base64 encoded string to array of bytes."
  ^"[B" [s]
  (codecs/b64->bytes (codecs/str->bytes (str s))))

(defn to-bytes
  "Converts object to bytes by converting it to a string first with UTF-8 encoding."
  ^"[B" [obj]
  (if (bytes? obj) obj (.getBytes ^String (str obj) "UTF-8")))

(def ^"[B" bzero
  "Returns zeroed array of bytes."
  (to-bytes nil))

(defn bytes-to-string
  "Converts bytes into a string"
  ^String [^"[B" b]
  (s/assert ::bytes b)
  (apply str (map #(char (bit-and % 255)) b)))

(defn bytes-concat
  "Concatenates byte arrays."
  ([]
   nil)
  (^"[B" [^"[B" bary]
   (not-empty bary))
  (^"[B" [^"[B" bary & byte-arys]
   (let [byte-arys (remove empty? (cons bary byte-arys))]
     (if (seq byte-arys)
       (let [sum-size          (apply + (map count byte-arys))
             ^"[B"       buff  (byte-array sum-size)
             ^ByteBuffer bbuff (ByteBuffer/wrap buff)]
         (doseq [^"[B" a byte-arys] (.put bbuff a))
         buff)))))

(defn text-to-bytes
  ^"[B" [t]
  (if (bytes? t) t (if (nil? t) bzero (to-bytes t))))

(defn normalize-to-bytes
  ^"[B" [t]
  (to-bytes (normalize-name t)))

;; Identifiers handling

(defn must-have-ns
  [id ^String ns]
  (s/assert ::not-empty-string ns)
  (s/assert ::identifier id)
  (if (and (qualified-ident? id) (= (namespace id) ns))
    id
    ((if (keyword? id) keyword symbol) ns (name id))))

(defn ensure-str
  (^String [v]
   (or (if (valuable? v) (str (if (ident? v) (symbol v) v))) ""))
  (^String [v & more]
   (apply str (map some-str (cons v more)))))

(defn ensure-ns
  "Takes an identifier `id` and a namespace `ns` (a string), and tries to set a
  namespace in the identifier. If a namespace already exists in identifier, it is not
  changed."
  [id ^String ns]
  (s/assert ::not-empty-string ns)
  (s/assert ::identifier id)
  (if (qualified-ident? id)
    id
    ((if (keyword? id) keyword symbol) ns (name id))))

(defn ensure-keyword
  "Takes an identifier `id` and tries to convert it to a keyword. If it is not an
  identifier, it calls `keyword`."
  [id]
  (if id
    (if (keyword? id)
      id
      (if (ident? id)
        (if (simple-ident? id)
          (keyword id)
          (keyword (namespace id) (name id)))
        (keyword (if (valuable? id) id))))))

(defn ensure-ident-keyword
  [id]
  (if (ident? id) id (ensure-keyword id)))

(defn ensure-keyword-having-ns
  [id ^String ns]
  (must-have-ns (ensure-keyword id) ns))

(defn ensure-namespaced-keyword
  [id ^String ns]
  (ensure-ns (ensure-keyword id) ns))

;; Control flow

(defmacro try-null
  "Evaluates body and if NullPointerException exception is caught it returns
  nil. Otherwise it returns the value of last expression in the body."
  {:added "1.0.0"}
  [& body]
  `(try ~@body
        (catch NullPointerException  e# nil)))

(defmacro when-not-empty
  "Evaluates body when the given value is a non-empty collection."
  {:added "1.0.0"}
  [val & body]
  `(when (seq ~val)
     ~@body))

(defn with-not-empty
  "Returns the collection if it's not empty. Otherwise returns `nil`."
  {:added "1.0.0"}
  [obj]
  (if (seq obj) obj))

(defmacro is
  "Takes a predicate `pred`, a value `val` and a body. Evaluates `val` and passes to
  `pred`. If the result is truthy it evaluates all expressions from body in an
  explicit `do`. Otherwise it returns the value."
  [pred val & body]
  `(let [v# ~val]
     (if (~pred v#) (do ~@body) v#)))

(defmacro is-not
  "Takes a predicate `pred`, a value `val` and a body. Evaluates `val` and passes to
  `pred`. If the result is truthy it returns the value. Otherwise it evaluates all
  expressions from body in an explicit `do`."
  [pred val & body]
  `(let [v# ~val]
     (if (~pred v#) v# (do ~@body))))

(defmacro or-some
  "Same as `or` but returns first value which is strictly not `nil`."
  ([] nil)
  ([x] x)
  ([x & next]
   `(let [or# ~x]
      (if (nil? or#) (or-some ~@next) or#))))

(defmacro qsome
  "Same as `clojure.core/some` but when `coll` is a constant form then a source code
  with `or` expression is generated instead of `some` with recurrent predicate
  application."
  [pred coll]
  (if (sequential? coll)
    (let [pred-sym (gensym "pred__qsome__")]
      `(let [~pred-sym ~pred]
         (or ~@(map #(list pred-sym %) coll))))
    `(some ~pred ~coll)))

(defn some-fn*
  "Same as `clojure.core/some-fn` but multiple arguments are passed to each predicate
  function and nullary variant is not returning `nil` but calls each predicate
  without passing any arguments.

  Takes a set of predicates and returns a function that returns the first truthy
  value (not `nil` and not `false`) returned by one of its composing predicates
  against all of its arguments, else it returns a value returned by the last
  predicate given (which may be `false` or `nil`)."
  ([p] p)
  ([p1 p2]
   (fn
     ([] (or (p1) (p2)))
     ([a] (or (p1 a) (p2 a)))
     ([a b] (or (p1 a b) (p2 a b)))
     ([a b c] (or (p1 a b c) (p2 a b c)))
     ([a b c d] (or (p1 a b c d) (p2 a b c d)))
     ([a b c d e] (or (p1 a b c d e) (p2 a b c d e)))
     ([a b c d e f] (or (p1 a b c d e f) (p2 a b c d e f)))
     ([a b c d e f & args] (or (apply p1 a b c d e f args) (apply p2 a b c d e f args)))))
  ([p1 p2 p3]
   (fn
     ([] (or (p1) (p2) (p3)))
     ([a] (or (p1 a) (p2 a) (p3 a)))
     ([a b] (or (p1 a b) (p2 a b) (p3 a b)))
     ([a b c] (or (p1 a b c) (p2 a b c) (p3 a b c)))
     ([a b c d] (or (p1 a b c d) (p2 a b c d) (p3 a b c d)))
     ([a b c d e] (or (p1 a b c d e) (p2 a b c d e) (p3 a b c d e)))
     ([a b c d e f] (or (p1 a b c d e f) (p2 a b c d e f) (p3 a b c e f)))
     ([a b c d e f & args] (or (apply p1 a b c d e f args)
                               (apply p2 a b c d e f args)
                               (apply p3 a b c d e f args)))))
  ([p1 p2 p3 p4]
   (fn
     ([] (or (p1) (p2) (p3) (p4)))
     ([a] (or (p1 a) (p2 a) (p3 a) (p4 a)))
     ([a b] (or (p1 a b) (p2 a b) (p3 a b) (p4 a b)))
     ([a b c] (or (p1 a b c) (p2 a b c) (p3 a b c) (p4 a b c)))
     ([a b c d] (or (p1 a b c d) (p2 a b c d) (p3 a b c d) (p4 a b c d)))
     ([a b c d e] (or (p1 a b c d e) (p2 a b c d e) (p3 a b c d e) (p4 a b c d e)))
     ([a b c d e f] (or (p1 a b c d e f) (p2 a b c d e f) (p3 a b c e f) (p4 a b c d e f)))
     ([a b c d e f & args] (or (apply p1 a b c d e f args)
                               (apply p2 a b c d e f args)
                               (apply p3 a b c d e f args)
                               (apply p4 a b c d e f args)))))
  ([p1 p2 p3 p4 & preds]
   (let [preds (list* p1 p2 p3 p4 preds)]
     (fn spn
       ([] (some #(%) preds))
       ([a] (some #(% a) preds))
       ([a b] (some #(% a b) preds))
       ([a b c] (some #(% a b c) preds))
       ([a b c d] (some #(% a b c d) preds))
       ([a b c d e] (some #(% a b c d e) preds))
       ([a b c d e f] (some #(% a b c d e f) preds))
       ([a b c d e f & args] (some #(apply % a b c d e f args) preds))))))

;; Namespace inference

(defn ns-infer
  "Takes a string of namespace name and a keyword. If the given keyword is not
  namespace-qualified it returns a new keyword with the given namespace added. If the
  given keyword is already equipped with a namespace it returns it."
  {:added "1.0.0" :tag clojure.lang.Keyword}
  ([^String ns-name
    ^clojure.lang.Keyword k]
   (if (simple-keyword? k)
     (keyword ns-name (name k))
     k))
  ([^String ns-name
    ^clojure.lang.Keyword k
    ^Boolean use-infer]
   (if use-infer (ns-infer ns-name k) k)))

(defn inferred-contains?
  "Just like the contains? but if the keyword is namespace-qualified it also checks if
  the collection contains the same keyword as its key but without a namespace."
  {:added "1.0.0" :tag Boolean}
  [^clojure.lang.IPersistentMap coll
   ^clojure.lang.Keyword k]
  (or (contains? coll k)
      (if (simple-keyword? k)
        false
        (contains? coll (keyword (name k))))))

(defn inferred-get
  "Just like the get function but if the keyword is namespace-qualified it first
  attempts to look for the value associated with it. If that fails it uses the
  variant of the keyword without any namespace."
  {:added "1.0.0"}
  ([^clojure.lang.IPersistentMap coll
    ^clojure.lang.Keyword k]
   (inferred-get coll k nil))
  ([^clojure.lang.IPersistentMap coll
    ^clojure.lang.Keyword k
    default]
   (if (simple-keyword? k)
     (k coll default)
     ((if (contains? coll k) k (keyword (name k))) coll default))))

;; Threads

(defn current-thread-id   [] (.. Thread currentThread getId))
(defn current-thread-name [] (.. Thread currentThread getName))
(defn current-thread      [] (Thread/currentThread))

;; Randomness

(defn get-rand-int
  "Like rand-int but optionally uses random number generator."
  {:added "1.0.0"}                      ; was: :tag 'int
  ([^long n]
   (if (some? n)
     (rand-int n)))
  ([^long n ^Random rng]
   (if (some? n)
     (if (nil? rng)
       (get-rand-int n)
       (if (zero? n) (int n) (.nextInt rng n))))))

(defn random-digits-len
  "For 0 or 1 it returns its argument. For other positive numbers it returns a random
  natural number from 1 to this number (inclusive) in 50% cases. In other 50% cases
  it returns its argument."
  {:added "1.0.0"} ; was: :tag 'long
  ([^long x
    ^long iteration
    ^Boolean shrink-now]
   (if (some? x)
     (if (zero? x) x
         (if-not shrink-now x
                 (if (zero? iteration) 1
                     (if (or (< iteration 6) (zero? (rand-int 2)))
                       (unchecked-inc (rand-int x)) x))))))
  ([^long x
    ^long iteration
    ^Boolean shrink-now
    ^Random rng]
   (if (some? x)
     (if (nil? rng)
       (random-digits-len x iteration shrink-now)
       (if (zero? x) x
           (if-not shrink-now x
                   (if (zero? iteration) 1
                       (if (or (< iteration 6) (zero? (get-rand-int 2 rng)))
                         (unchecked-inc (get-rand-int x rng)) x))))))))

(defn gen-digits
  "Generates the given number of random digits and converts all into a single string.
  When the second argument is present it should be an instance of random number
  generator used to get the digits."
  {:added "1.0.0" :tag String}
  ([^long num]
   (apply str (repeatedly num #(rand-int 10))))
  ([^long num
    ^Random rng]
   (if (some? num)
     (if (nil? rng)
       (gen-digits num)
       (apply str (repeatedly num #(.nextInt rng 10)))))))

;; Characters and digits

(defn count-digits
  "Returns a number of digits in a decimal number `n`."
  {:added "1.0.0" :tag 'long}
  [^long n]
  (if (zero? n) 1
      (unchecked-inc
       (long (Math/floor (Math/log10 n))))))

(defn char-ranges->set
  "Returns a set of characters defined as a collection of collections with start and
  stop character, e.g.: [\\A \\Z][\\0 \\9]"
  {:added "1.0.0" :tag clojure.lang.PersistentHashSet}
  [& ranges]
  (set (mapcat #(map char (range (byte (first %)) (inc (byte (second %))))) ranges)))

;; Sequences

(defn lazy-iterator-seq
  "Returns a lazy sequence as an interface to the given iterable Java object."
  {:added "1.0.0" :tag clojure.lang.LazySeq}
  ([^Iterable coll]
   (lazy-iterator-seq coll (.iterator coll)))
  ([^Iterable coll ^java.util.Iterator iter]
   (lazy-seq
    (if (.hasNext ^java.util.Iterator iter)
      (cons (.next ^java.util.Iterator iter)
            (lazy-iterator-seq ^Iterable coll ^java.util.Iterator iter))))))

(defn juxt-seq
  "Like `clojure.core/juxt` but produces lazy sequence of results instead of a vector."
  ^clojure.lang.LazySeq [& functions]
  (fn [& args]
    (map #(apply %1 %2) functions (repeat args))))

(defn insert-at
  "Takes an index number `index`, a collection `coll` and an element `element`, and
  inserts element's value under the given index number. Uses sequential operations:
  `split-at`, `concat` and `cons`."
  [index coll element]
  (let [[l r] (split-at index coll)]
    (concat l (cons element r))))

(defn find-first
  "Returns the first item from coll for which (f item) returns true or nil if no such
  item is present. If the given not-found value is supplied, it will return it
  instead of nil."
  ([f coll]
   (find-first f coll nil))
  ([f coll not-found]
   (reduce (fn [_ x] (if (f x) (reduced x) not-found))
           not-found coll)))

;; Indexed collections

(defn contains-some?
  "Takes two indexed collections and returns true if at least one element is shared in
  both. Otherwise it returns false."
  [s1 s2]
  (or (if (> (count s1) (count s2))
        (some #(contains? s1 %) s2)
        (some #(contains? s2 %) s1))
      false))

;; UUID

(def ^{:arglists '(^java.util.UUID []) :tag java.util.UUID}
  random-uuid
  (or (ns-resolve 'clojure.core 'random-uuid)
      (fn ^java.util.UUID [] (java.util.UUID/randomUUID))))

(defn to-uuid
  "Converts the given value to UUID. If it's empty or `nil`, returns `nil`."
  ([]
   (random-uuid))
  ([s]
   (if (valuable? s) (if (uuid? s) s (UUID/fromString (str s))))))

(def uuid
  (or (ns-resolve 'clojure.core 'uuid)
      to-uuid))

;; URL

(defn sanitize-base-url
  [^String url]
  (if-some [url (str/trim (str url))]
    (if (some? (seq url))
      (let [url (if (str/starts-with? url "http") url (str "https://" url))
            url (if (str/ends-with?   url    "/") url (str url "/"))]
        url))))

(defn parse-url
  "Parses URL into a map."
  [u]
  (if (and u (string? u) (> (count u) 0))
    (http/parse-url u)))

;; Numbers

(defn- parse-long-java
  ^Long [^String s]
  (Long/parseLong ^String s))

(def ^{:private  true
       :tag      Long
       :arglists '(^Long [^String v])}
  parse-long-core
  (or (ns-resolve 'clojure.core 'parse-long)
      parse-long-java))

(defn pos-val
  "Returns the given value `x` if it is a positive number. Otherwise it returns `nil`."
  [x]
  (if (and x (number? x) (pos? x)) x))

(defn parse-num
  ([n default]
   (or (parse-num n) default))
  ([n]
   (if (valuable? n)
     (let [s (str n)]
       (if (or (> (count s) 15) (str/index-of s \.))
         (bigdec ^String s)
         (parse-long-core ^String s))))))

(defn some-long
  ([s default]
   (or (some-long s) default))
  ([s]
   (if (valuable? s)
     (if (number? s) (long s)
         (parse-long-core ^String (str s))))))

(def ^{:arglists '([s] [s default])}
  parse-long
  some-long)

(defn safe-parse-num
  ([v default]
   (or (safe-parse-num v) default))
  ([v]
   (try (parse-num v)
        (catch Throwable e nil))))

(defn safe-parse-long
  ([v default]
   (or (safe-parse-long v) default))
  ([v]
   (try (some-long v)
        (catch Throwable e nil))))

(defn to-long
  [s default]
  (s/assert ::integer default)
  (safe-parse-long s default))

(defn parse-percent
  ([n default]
   (or (parse-percent n) default))
  ([n]
   (if-some [n (parse-num n)] (/ n 100))))

(def percent parse-percent)

(defn safe-parse-percent
  ([v default]
   (or (safe-parse-percent v) default))
  ([v]
   (try (parse-percent v)
        (catch Throwable e nil))))

(defn parse-re
  [v]
  (if (and (valuable? v) (string? v)) (re-pattern v)))

;; Identifiers

(defn some-keyword
  [v]
  (if (keyword? v) v
      (if (valuable? v)
        (keyword (if (symbol? v) v (str v))))))

(defn some-keyword-up
  [v]
  (if (valuable? v)
    (keyword
     (str/upper-case
      (str (if (keyword? v) (symbol v) v))))))

(defn some-keyword-simple
  [v]
  (if-some [v (some-keyword v)]
    (if (simple-keyword? v) v (keyword (name v)))))

(defn simple-keyword-up
  [v]
  (if-some [v (some-keyword-up v)]
    (if (simple-keyword? v) v (keyword (name v)))))

(defn some-symbol
  [v]
  (if (symbol? v) v
      (if (valuable? v)
        (symbol (if (ident? v) v (str v))))))

(defn some-symbol-up
  [v]
  (if (valuable? v)
    (symbol
     (str/upper-case
      (str (if (keyword? v) (symbol v) v))))))

(defn some-symbol-simple
  [v]
  (if-some [v (some-symbol v)]
    (if (simple-symbol? v) v (symbol (name v)))))

(defn simple-symbol-up
  [v]
  (if-some [v (some-symbol-up v)]
    (if (simple-symbol? v) v (symbol (name v)))))

;; Namespaces and global identifiers

(defn try-require
  "Tries to require namespace `n` and returns the given argument. If the file does not
  exists, returns `nil`."
  [n]
  (if n
    (if-some [n (if (ident? n) n (some-str n))]
      (if-some [n (symbol n)]
        (try (do (require n) n)
             (catch java.io.FileNotFoundException _))))))

(defn fn-name
  "Tries to obtain symbolic function name for the given function `f`. Uses metadata and
  Java reflection as fallback."
  [f]
  (let [{mna :name mns :ns} (meta f)]
    (if (and mna mns)
      (symbol (str mns) (str mna))
      (as-> (str f) $
        (cmain/demunge $)
        (or (re-find #"(.+)--\d+@" $) (re-find #"(.+)@" $))
        (last $)
        (symbol $)))))

;; Function arguments

(defn mapply
  "Like apply but works on named arguments. Takes function f and a list of
  arguments to be passed, were the last argument should be a map that will be
  decomposed and passed as named arguments.

  Returns the result of calling f."
  [f & args]
  (apply f (concat (butlast args) (mapcat identity (last args)))))

;; Console

(defn read-line-with-prompt
  "Reads a line of text from console with optional prompt. Returns a string or `nil`
  when the entered string is empty."
  ([]
   (some-str (read-line)))
  ([prompt]
   (print prompt)
   (flush)
   (some-str (read-line))))

(defn ask
  "Ask user for a string with optional confirmation using ask-fn to get a string (or nil).
  Repeats until two entered strings are the same and are not empty.
  Keyword arguments can be given to configure behavior:
  `prompt` (message displayed when asking for first string),
  `confirm-prompt` (message displayed when asking for the same string again),
  `not-match-msg` (message displayed when strings do not match),
  `empty-msg` (message displayed when the entered string is empty),
  `retries` (number of retries before quitting the loop; when set to `nil` or not
  given, it will continue indefinitely),
  `confirmation?` (requires string to be re-entered for confirmation, defaults to `true`),
  `allow-empty?` (allows the entered string to be empty; defaults to `false`),
  `empty-nil?` (returns `nil` instead of an empty string; defaults to `false`),
  `empty-quits?` (short-circuits on any empty string and returns `nil`; defaults to `false`),
  `empty-quits-nil?` (returns `nil` when quitting on empty string; defaults to `true`).
  Returns the entered string or `nil`."
  ([& {:keys [ask-fn
              allow-empty?
              empty-nil?
              empty-quits?
              empty-quits-nil?
              prompt
              confirm-prompt
              not-match-msg
              empty-msg
              retries
              confirmation?]
       :or   {ask-fn           read-line-with-prompt
              allow-empty?     false
              empty-nil?       false
              empty-quits?     false
              empty-quits-nil? true
              confirmation?    true
              prompt           "Enter text: "
              confirm-prompt   "Repeat text: "
              not-match-msg    "Texts do not match."
              empty-msg        "Text is empty."}}]
   (loop [counter (if retries (unchecked-int (if (pos-int? retries) retries 1)))]
     (if-not (and counter (zero? counter))
       (let [p1 (ask-fn prompt)]
         (if (and (nil? p1) empty-quits?)
           (if-not empty-quits-nil? "")
           (let [p2      (if confirmation? (ask-fn confirm-prompt) p1)
                 counter (if counter (unchecked-dec-int counter))]
             (if (and (nil? p2) empty-quits?)
               (if-not empty-quits-nil? "")
               (if (= p1 p2)
                 (or p1 (if allow-empty? (if-not empty-nil? "") (do (println empty-msg) (recur counter))))
                 (do (println not-match-msg) (recur counter)))))))))))

;; Documentation strings

(defmacro defdoc! [v docstr]
  "Replaces documentation string of a Var."
  `(alter-meta! (var ~v) #(update-in % [:doc] (constantly (str ~docstr)))))
