(ns

    ^{:doc    "Random Utilities, user identity record type."
      :author "Paweł Wilk"
      :added  "2.0.7"}

    io.randomseed.utils.identity.types)

(defprotocol IdentityString
  "Protocol used by `Identity/toString` to decouple stringification strategy from
  the record namespace."
  (^{:tag String}
   -identity-str
   [identity]))

(defrecord Identity [^clojure.lang.Keyword id-type value]
  Object
  (toString ^String [^Identity i] (-identity-str i)))

(extend-type Identity
  IdentityString
  (^String -identity-str [^Identity i]
   (str (.value i))))
