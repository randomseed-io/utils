(ns

    ^{:doc    "Random utils, common validators."
      :author "Paweł Wilk"
      :added  "1.0.0"}

    io.randomseed.utils.validators.common

  (:refer-clojure :exclude [parse-long uuid random-uuid])

  (:import [org.apache.commons.validator.routines
            EmailValidator
            UrlValidator
            DateValidator]
           [java.util Locale])

  (:require [trptr.java-wrapper.locale :as          l]
            [phone-number.core         :as      phone]
            [io.randomseed.utils.map   :as        map]
            [io.randomseed.utils       :refer    :all]))

;;
;; e-mail
;;

(defn valid-email?
  (^Boolean [e]
   (valid-email? e false true))
  (^Boolean [e allow-local]
   (valid-email? e allow-local true))
  (^Boolean [e allow-local allow-tld]
   (boolean
    (when-some [e (some-str e)]
      (.isValid ^EmailValidator (EmailValidator/getInstance ^Boolean (boolean allow-local)
                                                            ^Boolean (boolean allow-tld))
                ^String e)))))

(defn valid-email-domain?
  (^Boolean [e]
   (valid-email-domain? e false true))
  (^Boolean [e allow-local]
   (valid-email-domain? e allow-local true))
  (^Boolean [e allow-local allow-tld]
   (boolean
    (when-some [e (some-str e)]
      (.isValidDomain ^EmailValidator (EmailValidator/getInstance ^Boolean (boolean allow-local)
                                                                  ^Boolean (boolean allow-tld))
                      ^String e)))))

(defn valid-email-local-part?
  (^Boolean [e]
   (valid-email-local-part? e false true))
  (^Boolean [e allow-local]
   (valid-email-local-part? e allow-local true))
  (^Boolean [e allow-local allow-tld]
   (boolean
    (when-some [e (some-str e)]
      (.isValidUser ^EmailValidator (EmailValidator/getInstance ^Boolean (boolean allow-local)
                                                                ^Boolean (boolean allow-tld))
                    ^String e)))))

;;
;; date
;;

(defn valid-date?
  (^Boolean [value]
   (boolean
    (when-some [value (some-str value)]
      (.isValid ^DateValidator (.getInstance DateValidator) ^String value))))
  (^Boolean [value pattern]
   (boolean
    (when-some [value (some-str value)]
      (if-some [pattern (some-str pattern)]
        (.isValid ^DateValidator (.getInstance DateValidator)
                  ^String value ^String pattern)
        (.isValid ^DateValidator (.getInstance DateValidator)
                  ^String value)))))
  (^Boolean [value pattern locale]
   (boolean
    (when-some [value (some-str value)]
      (if-some [pattern (some-str pattern)]
        (if locale
          (.isValid ^DateValidator (.getInstance DateValidator)
                    ^String value ^String pattern ^Locale (l/locale locale))
          (.isValid ^DateValidator (.getInstance DateValidator)
                    ^String value ^String pattern))
        (if locale
          (.isValid ^DateValidator (.getInstance DateValidator)
                    ^String value
                    ^Locale (l/locale locale))
          (.isValid ^DateValidator (.getInstance DateValidator)
                    ^String value)))))))

;;
;; phone number
;;

(defn valid-phone?
  (^Boolean [p]    (phone/valid? p))
  (^Boolean [p dr] (phone/valid? p dr)))

(defn valid-regular-phone?
  (^Boolean [p]
   (and (phone/valid? p)
        (not (let [pn (phone/number p)]
               (or (phone/is-short? pn)
                   (contains? #{:phone-number.type/toll-free
                                :phone-number.type/premium-rate
                                :phone-number.type/shared-cost}
                              (phone/type pn)))))))
  (^Boolean [p dr]
   (and (phone/valid? p dr)
        (not (let [pn (phone/number p dr)]
               (or (phone/is-short? pn)
                   (contains? #{:phone-number.type/toll-free
                                :phone-number.type/premium-rate
                                :phone-number.type/shared-cost}
                              (phone/type pn))))))))

;;
;; url
;;

(defn valid-url?
  ^Boolean [u]
  (boolean
   (when-some [u (some-str u)]
     (.isValid ^UrlValidator (UrlValidator/getInstance) ^String u))))
