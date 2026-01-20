(ns io.randomseed.utils.bot-test

  (:require [clojure.test :refer [deftest is testing]]
            [io.randomseed.utils.bot :as bot]))

(deftest bot-ns-always-a-symbol
  (testing "qualified keyword -> symbol namespace+name"
    (is (= 'my.custom.bot (bot/bot-ns :my.custom/bot :fallback))))

  (testing "string -> symbol"
    (is (= 'my.custom.ns (bot/bot-ns "my.custom.ns" :fallback))))

  (testing "map with :ns already a symbol"
    (is (= 'my.custom.ns (bot/bot-ns 'my.custom.ns :fallback))))

  (testing "nil falls back to id"
    (is (= 'my.custom.bot (bot/bot-ns nil :my.custom/bot)))))

(deftest bot?-shape
  (testing "bot? is a pure shape predicate"
    (is (true? (bot/bot? {:bot :x :ns 'x})))
    (is (false? (bot/bot? {:bot nil :ns 'x})))
    (is (false? (bot/bot? {:bot :x})))
    (is (false? (bot/bot? nil)))))

(deftest instance-config-validation
  (testing "instances present without instance-id is rejected"
    (is (thrown? AssertionError
                 (bot/instance-config {:ns        'a.b
                                       :bot       :x
                                       :instances {:one {:x 1}}}
                                      nil))))

  (testing "instance-id must point to a non-empty instance"
    (is (thrown? AssertionError
                 (bot/instance-config {:ns        'a.b
                                       :bot       :x
                                       :instances {:one nil}}
                                      :one))))

  (testing "valid instance-id adds :instance and merges instance config"
    (let [cfg (bot/instance-config {:ns        'a.b
                                    :bot       :x
                                    :instances {:one {:x 1}}}
                                   :one)]
      (is (= :a.b/one (:instance cfg)))
      (is (= 1 (:x cfg))))))

(deftest parse-config-minimal
  (testing "parse-config returns nil when :bot is missing"
    (is (nil? (bot/parse-config {}))))

  (testing "parse-config fills derived keys and can instantiate when :instances is present"
    (let [cfg (bot/parse-config {:bot       :gamma
                                 :ns        :my.custom/ns
                                 :instances {:one {:x 1}}}
                                :one)]
      (is (= :io.randomseed.utils.bot/gamma (:bot cfg)))      (is (symbol? (:ns cfg)))
      (is (= :my.custom.ns/one (:instance cfg)))
      (is (= 1 (:x cfg)))
      (is (nil? (:instances cfg))) ; bo instance-config robi (dissoc :instances)
      (is (bot/bot? cfg)))))
