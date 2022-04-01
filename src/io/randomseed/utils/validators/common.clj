(ns

    ^{:doc    "Random utils, common validators."
      :author "Paweł Wilk"
      :added  "1.0.0"}

    io.randomseed.utils.validators.common

  (:refer-clojure :exclude [parse-long uuid random-uuid])

  (:import [org.apache.commons.validator.routines
            EmailValidator
            UrlValidator
            DateValidator])

  (:require
   [trptr.java-wrapper.locale :as          l]
   [phone-number.core         :as      phone]
   [io.randomseed.utils.map   :as        map]
   [io.randomseed.utils       :refer    :all]))

;;
;; e-mail
;;

(defn valid-email?
  (^Boolean [address]
   (valid-email? address false false))
  (^Boolean [address ^Boolean allow-local]
   (valid-email? address allow-local false))
  (^Boolean [address ^Boolean allow-local ^Boolean allow-tld]
   (and
    (string? address)
    (when-some [v (.getInstance EmailValidator allow-local allow-tld)]
      (.isValid v address)))))

(defn ^Boolean valid-email-domain?
  ([address]
   (valid-email-domain? address false false))
  ([address ^Boolean allow-local]
   (valid-email-domain? address allow-local false))
  ([address ^Boolean allow-local ^Boolean allow-tld]
   (and
    (string? address)
    (when-some [v (.getInstance EmailValidator allow-local allow-tld)]
      (.isValidDomain v address)))))

(defn valid-email-local-part?
  (^Boolean [address]
   (valid-email-local-part? address false false))
  (^Boolean [address ^Boolean allow-local]
   (valid-email-local-part? address allow-local false))
  (^Boolean [address ^Boolean allow-local ^Boolean allow-tld]
   (and
    (string? address)
    (when-some [v (.getInstance EmailValidator allow-local allow-tld)]
      (.isValidUser v address)))))

;;
;; date
;;

(defn ^Boolean valid-date?
  ([value]
   (and
    (string? value)
    (when-some [v (.getInstance DateValidator)]
      (.isValid v value))))
  ([value pattern]
   (and
    (string? value)
    (string? pattern)
    (when-some [v (.getInstance DateValidator)]
      (.isValid v value pattern))))
  ([value pattern ^java.util.Locale locale]
   (and
    (string? value)
    (when-some [v (.getInstance DateValidator)]
      (if (nil? pattern)
        (.isValid v value (l/locale locale))
        (and (string? pattern) (.isValid v value pattern (l/locale locale))))))))

;;
;; phone number
;;

(defn valid-phone?
  ^Boolean [number]
  (phone/valid? number))
