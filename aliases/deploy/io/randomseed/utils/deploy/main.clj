(ns io.randomseed.utils.deploy.main

  (:require [deps-deploy.deps-deploy :as dd]))

(defn deploy
  "Usage:
    clj -T:deploy deploy :artifact '\"target/utils-1.0.0.jar\"'
  Optional:
    :sign? true|false
    :sign-key-id '\"<fingerprint>\"'
  Env:
    CLOJARS_USERNAME, CLOJARS_PASSWORD (already in bin/deploy)"
  [{:keys [artifact sign? sign-key-id installer]
    :or   {sign?     true
           installer :remote}}]
  (when-not artifact
    (throw (ex-info "Missing :artifact (path to jar)." {})))
  (dd/deploy (cond-> {:installer      installer
                      :artifact       artifact
                      :sign-releases? (boolean sign?)}
               sign-key-id (assoc :sign-key-id sign-key-id))))
