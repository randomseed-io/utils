(ns

    ^{:doc    "Random utils, var tests."
      :author "Paweł Wilk"
      :added  "1.0.0"
      :no-doc true}

    io.randomseed.utils.var-test

  (:refer-clojure :exclude [parse-long uuid random-uuid])

  (:require [clojure.spec.alpha              :as               s]
            [clojure.spec.gen.alpha          :as             gen]
            [orchestra.spec.test             :as              st]
            [io.randomseed.utils             :refer         :all]
            [expound.alpha                   :as         expound]
            [io.randomseed.utils.var         :as             var]))

(s/check-asserts true)
