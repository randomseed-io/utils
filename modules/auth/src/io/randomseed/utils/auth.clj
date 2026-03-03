(ns

    ^{:doc    "Random Utilities, authentication entry point."
      :author "Paweł Wilk"
      :added  "2.0.6"}

    io.randomseed.utils.auth

  (:refer-clojure :exclude [parse-long uuid random-uuid])

  (:require [io.randomseed.utils.auth.types :as types]
            [io.randomseed.utils            :as utils]
            [io.randomseed.utils.auth.pwd   :as   pwd]
            [io.randomseed.utils.time       :as  time]
            [io.randomseed.utils.var        :as   var]))

(defonce ^{:redef true
            :doc   "Holds the current authentication setup. Intended to be re-bound at init time."}
  setup nil)

(declare make-passwords)

;;
;; Access to settings and configuration
;;

(defn settings
  "Returns authentication settings map for a source `src`."
  [src]
  (let [resolved (if (or (symbol? src) (string? src) (var? src))
                   (var/deref src)
                   src)]
    (cond
      (map? resolved) resolved
      :else           resolved)))

(defn settings?
  "Returns `true` when `v` can be treated as auth settings map."
  [v]
  (map? v))

(defn- resolve-config
  [cfg]
  (cond
    (map? cfg) cfg
    :else      nil))

(defn config
  "Returns authentication configuration."
  ([src]
   (let [s (settings src)]
     (or (resolve-config (:default s))
         (resolve-config (:config s))
         (when (map? s)
           (resolve-config s)))))
  ([src _]
   (config src)))

;;
;; Password authentication
;;

(defn- auth-passwords
  [auth-config]
  (let [cfg       (resolve-config auth-config)
        passwords (:passwords cfg)]
    (cond
      (map? passwords) (make-passwords passwords)
      :else            nil)))

(defn check-password
  "Checks plain `password` against suite(s) using `auth-config`."
  ([password pwd-suites auth-config]
   (when (and password pwd-suites auth-config)
     (when-some [checker (:check (auth-passwords auth-config))]
       (if (map? pwd-suites)
         (checker password pwd-suites)
         (checker password nil pwd-suites)))))
  ([password pwd-shared-suite pwd-user-suite auth-config]
   (when (and password pwd-shared-suite pwd-user-suite auth-config)
     (when-some [checker (:check (auth-passwords auth-config))]
       (checker password pwd-shared-suite pwd-user-suite)))))

(defn check-password-json
  "Checks plain `password` against JSON suite(s) using `auth-config`."
  ([password json-pwd-suites auth-config]
   (when (and password json-pwd-suites auth-config)
     (when-some [checker (:check-json (auth-passwords auth-config))]
       (if (map? json-pwd-suites)
         (checker password json-pwd-suites)
         (checker password nil json-pwd-suites)))))
  ([password json-pwd-shared-suite json-pwd-user-suite auth-config]
   (when (and password json-pwd-shared-suite json-pwd-user-suite auth-config)
     (when-some [checker (:check-json (auth-passwords auth-config))]
       (checker password json-pwd-shared-suite json-pwd-user-suite)))))

(defn make-password
  "Creates encrypted password suite."
  [password auth-config]
  (when (and password auth-config)
    (when-some [encryptor (:encrypt (auth-passwords auth-config))]
      (encryptor password))))

(defn make-password-json
  "Creates encrypted password suite in JSON form."
  [password auth-config]
  (when (and password auth-config)
    (when-some [encryptor (:encrypt-json (auth-passwords auth-config))]
      (encryptor password))))

;;
;; Settings initialization
;;

(defn make-passwords
  "Builds `AuthPasswords` record from passwords config map."
  [m]
  (let [m (when (map? m) (or (:passwords m) m))]
    (when (map? m)
      (types/->AuthPasswords
       (:id               m)
       (:suite            m)
       (or (:check        m) (:check-fn        m))
       (or (:check-json   m) (:check-json-fn   m))
       (or (:encrypt      m) (:encrypt-fn      m))
       (or (:encrypt-json m) (:encrypt-json-fn m))
       (or (:wait         m) (:wait-fn         m))))))

(defn make-locking
  "Builds `AuthLocking` record from auth config map."
  [m]
  (types/->AuthLocking
   (utils/safe-parse-long (:locking/max-attempts m) 10)
   ((fnil time/parse-duration [10 :minutes]) (:locking/lock-wait m))
   ((fnil time/parse-duration [1  :minutes]) (:locking/fail-expires m))))

(defn make-auth
  "Builds normalized auth config map with `:id`, `:passwords` and `:locking`."
  ([m]
   (make-auth nil m))
  ([k m]
   (let [m (or m {})]
     (assoc m
            :id        (utils/some-keyword-simple (or (:id m) k))
            :passwords (make-passwords m)
            :locking   (make-locking m)))))

(defn init-auth
  "Authentication configurator."
  [k config]
  (make-auth k config))

(defn init-passwords
  "Initializes password suite from config."
  ([k config]
   (init-passwords k config nil))
  ([k config log-fn]
   (make-passwords (pwd/init k config log-fn))))

(defn init-config
  "Prepares authentication settings."
  [config]
  (let [config  (or config {})
        default (or (:default config)
                    (:config config)
                    config)]
    (assoc config
           :default (make-auth (:id default) default))))
