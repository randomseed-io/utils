(ns user
  (:require
   [clojure.spec.alpha               :as                s]
   [orchestra.spec.test              :as               st]
   [clojure.spec.test.alpha          :as              cst]
   [clojure.spec.gen.alpha           :as              gen]
   [clojure.string                   :as              str]
   [clojure.repl                     :refer          :all]
   [clojure.test                     :refer [run-tests
                                             run-all-tests]]
   [clojure.tools.namespace.repl     :refer [refresh
                                             refresh-all]]
   [expound.alpha                    :as          expound]
   [taoensso.nippy                   :as            nippy]

   [io.randomseed.utils              :as             util]
   [io.randomseed.utils.fs           :as               fs]
   [io.randomseed.utils.db           :as               db]
   [io.randomseed.utils.ip           :as               ip]
   [io.randomseed.utils.var          :as              var]
   [io.randomseed.utils.map          :as              map]
   [io.randomseed.utils.vec          :as              vec]
   [io.randomseed.utils.set          :as              set]
   [io.randomseed.utils.log          :as              log]
   [io.randomseed.utils.bus          :as              bus]
   [io.randomseed.utils.bot          :as              bot]
   [io.randomseed.utils.time         :as             time]
   [io.randomseed.utils.crypto       :as           crypto]
   [io.randomseed.utils.nop-cache    :as        nop-cache]
   [io.randomseed.utils.validators   :as       validators]

   [clojure.core.async               :as             async]
   [clojure.core.async               :refer  [<! <!! >! >!!
                                              alts! alts!!
                                              chan close!
                                              go go-loop
                                              put! thread]]

   [tick.core                         :as                t]
   [puget.printer                     :as            puget]
   [puget.printer                     :refer      [cprint]]
   [kaocha.repl                       :refer          :all]))

(set! *warn-on-reflection* true)

(alter-var-root
 #'s/*explain-out*
 (constantly
  (expound/custom-printer {:show-valid-values? false
                           :print-specs?        true
                           :theme    :figwheel-theme})))

(when (System/getProperty "nrepl.load")
  (require 'nrepl)
  ;;(require 'infra)
  )

(st/instrument)

(defn test-all []
  (refresh)
  (cst/with-instrument-disabled
    (run-all-tests)))

(comment 
  (refresh-all)
  (cst/with-instrument-disabled (test-all))
  (cst/with-instrument-disabled (run-all))
  )
