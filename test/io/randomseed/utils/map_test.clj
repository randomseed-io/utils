(ns io.randomseed.utils.map-test

  (:require   [clojure.string]
              [clojure.test                    :refer [deftest is testing]]
              [clojure.test.check.clojure-test :refer [defspec]]
              [clojure.test.check.generators   :as                     gen]
              [clojure.test.check.properties   :as                    prop]
              [io.randomseed.utils.map         :as                       m]))

;; ---------------------------------------------------------------------------
;; Small helpers / macros
;; ---------------------------------------------------------------------------

(deftest lazy-get-laziness-and-semantics
  (testing "default expression is evaluated only when key missing"
    (let [calls (atom 0)
          mp    {:a 1}
          v1    (m/lazy-get mp :a (do (swap! calls inc) 42))
          v2    (m/lazy-get mp :b (do (swap! calls inc) 42))]
      (is (= 1 v1))
      (is (= 42 v2))
      (is (= 1 @calls))))
  (testing "when target is not associative, default is taken"
    (let [calls (atom 0)
          v     (m/lazy-get 123 :a (do (swap! calls inc) :fallback))]
      (is (= :fallback v))
      (is (= 1 @calls)))))

(deftest or-map-basics
  (is (= {} (m/or-map nil)))
  (let [mp {:a 1}]
    (is (identical? mp (m/or-map mp)))))

;; ---------------------------------------------------------------------------
;; qassoc / qupdate
;; ---------------------------------------------------------------------------

(deftest qassoc-basic
  (testing "nil map creates a new map"
    (is (= {:a 1} (m/qassoc nil :a 1)))
    (is (= {:a 1 :b 2} (m/qassoc nil :a 1 :b 2))))
  (testing "works on vectors too (Associative)"
    (is (= [0 42 2]
           (m/qassoc [0 1 2] 1 42))))
  (testing "variadic path throws for odd extra args"
    ;; Trigger the variadic arity (needs at least 9 pairs + rest).
    (is (thrown-with-msg?
         IllegalArgumentException
         #"even number"
         (apply m/qassoc {}
                [:a 1 :b 2 :c 3 :d 4 :e 5 :f 6 :g 7 :h 8 :i 9 :j])))))

(defspec qassoc-matches-core-assoc
  200
  (prop/for-all
   [base (gen/frequency [[1 (gen/return nil)]
                         [9 (gen/map gen/keyword gen/small-integer)]])
    kvs  (gen/vector (gen/tuple gen/keyword gen/small-integer) 1 20)]
   (let [flat-kvs (vec (mapcat identity kvs))]
     (= (apply assoc base flat-kvs)
        (apply m/qassoc base flat-kvs)))))

(deftest qupdate-basics
  (testing "updates existing key"
    (is (= {:a 2}
           (m/qupdate {:a 1} :a (fnil inc 0)))))
  (testing "missing key passes nil to f"
    (is (= {:a 1}
           (m/qupdate {} :a (fn [old] (if (nil? old) 1 old))))))
  (testing "variadic f receives args"
    (is (= {:a 7}
           (m/qupdate {:a 1} :a (fnil + 0) 2 4)))))

;; ---------------------------------------------------------------------------
;; Conditional assoc/update helpers
;; ---------------------------------------------------------------------------

(deftest assoc-missing-and-existing
  (testing "assoc-missing"
    (is (nil? (m/assoc-missing)))
    (is (= {:a 1} (m/assoc-missing {:a 1})))
    (is (= {:a 1 :b 2}
           (m/assoc-missing {:a 1} :b 2)))
    (is (= {:a 1}
           (m/assoc-missing {:a 1} :a 99)))
    (is (= {:a 1 :b 2}
           (m/assoc-missing nil :a 1 :b 2))))
  (testing "assoc-existing"
    (is (nil? (m/assoc-existing)))
    (is (= {:a 1} (m/assoc-existing {:a 1})))
    (is (= {:a 99}
           (m/assoc-existing {:a 1} :a 99)))
    (is (= {:a 1}
           (m/assoc-existing {:a 1} :b 2)))))

(deftest update-existing-and-missing
  (testing "update-existing skips missing keys"
    (is (= {:a 1}
           (m/update-existing {:a 1} :b inc))))
  (testing "update-missing updates only when key absent"
    (is (= {:a 1 :b 2}
           (m/update-missing {:a 1} :b (constantly 2))))
    (is (= {:a 1}
           (m/update-missing {:a 1} :a (constantly 2)))))
  (testing "non-function fun is treated as constant"
    (is (= {:a 5}
           (m/update-existing {:a 1} :a 5)))))

(deftest update-if-and-update-if-not
  (testing "update-if only when key exists AND pred passes"
    (is (= {:a 10}
           (m/update-if {:a 1} :a odd? * 10)))
    (is (= {:a 2}
           (m/update-if {:a 2} :a odd? * 10)))
    (is (= {:a 1}
           (m/update-if {:a 1} :b odd? * 10))))
  (testing "update-if-not only when key exists AND pred fails"
    (is (= {:a 1}
           (m/update-if-not {:a 1} :a odd? (constantly 0))))
    (is (= {:a 0}
           (m/update-if-not {:a 2} :a odd? (constantly 0))))))

(deftest update-to-bytes-and-back
  (testing "update-to-bytes converts only non-bytes"
    (let [b (.getBytes "x" "UTF-8")
          m1 (m/update-to-bytes {:a "x"} :a)
          m2 (m/update-to-bytes {:a b} :a)]
      (is (bytes? (:a m1)))
      (is (identical? b (:a m2)))))
  (testing "update-bytes-to-strings converts bytes"
    (let [b (.getBytes "x" "UTF-8")
          m1 (m/update-bytes-to-strings {:a b} :a)
          m2 (m/update-bytes-to-strings {:a "x"} :a)]
      (is (= "x" (:a m1)))
      (is (= "x" (:a m2))))))

(deftest assoc-if-macros-evaluate-once
  (testing "assoc-if evaluates coll expression only once"
    (let [calls (atom 0)
          coll  (fn [] (swap! calls inc) {:a 1})]
      (is (= {:a 1 :b 2}
             (m/assoc-if (coll) true :b 2)))
      (is (= 1 @calls))))
  (testing "assoc-if-not evaluates coll expression only once"
    (let [calls (atom 0)
          coll  (fn [] (swap! calls inc) {:a 1})]
      (is (= {:a 1}
             (m/assoc-if-not (coll) true :b 2)))
      (is (= 1 @calls)))))

(deftest assoc-if-key-macros
  (testing "assoc-if-key uses predicate on current value"
    (is (= {:a 10}
           (m/assoc-if-key {:a 1} :a odd? 10)))
    (is (= {:a 2}
           (m/assoc-if-key {:a 2} :a odd? 10))))
  (testing "assoc-if-not-key inverse"
    (is (= {:a 1}
           (m/assoc-if-not-key {:a 1} :a odd? 10)))
    (is (= {:a 10}
           (m/assoc-if-not-key {:a 2} :a odd? 10))))
  (testing "key expression is evaluated once"
    (let [calls (atom 0)
          k     (fn [] (swap! calls inc) :a)]
      (is (= {:a 10}
             (m/assoc-if-not-key {:a 2} (k) odd? 10)))
      (is (= 1 @calls)))))

;; ---------------------------------------------------------------------------
;; (dis)assoc / remove helpers
;; ---------------------------------------------------------------------------

(deftest dissoc-and-remove-helpers
  (testing "dissoc-if"
    (is (= {:a 1}
           (m/dissoc-if {:a 1 :b 2} :b even?)))
    (is (= {:a 1 :b 2}
           (m/dissoc-if {:a 1 :b 2} :b odd?))))
  (testing "remove-if-value"
    (is (= {:a 1}
           (m/remove-if-value {:a 1 :b 2} even?))))
  (testing "remove-if-value-in / not-in"
    (is (= {:a 1}
           (m/remove-if-value-in {:a 1 :b 2 :c 3} [2 3])))
    (is (= {:b 2 :c 3}
           (m/remove-if-value-not-in {:a 1 :b 2 :c 3} [2 3])))
    (is (= {:a 1}
           (m/remove-if-value-in {:a 1} nil)))
    (is (= {}
           (m/remove-if-value-not-in {:a 1} nil))))
  (testing "remove-except"
    (is (= {:a 1 :c 3}
           (m/remove-except {:a 1 :b 2 :c 3} [:a :c]))))
  (testing "remove-empty-values removes nil and empty seqables"
    ;; Note: empty string is seqable, so it is removed.
    (is (= {:c [1]}
           (m/remove-empty-values {:a nil :b [] :c [1] :d ""})))) )

;; ---------------------------------------------------------------------------
;; Map transforms
;; ---------------------------------------------------------------------------

(deftest map-transformers
  (testing "map-vals-by-k / map-vals-by-kv / map-vals"
    (is (= {:a :a :b :b}
           (m/map-vals-by-k identity {:a 1 :b 2} {})))
    (is (= {:a [:a 1] :b [:b 2]}
           (m/map-vals-by-kv vector {:a 1 :b 2} {})))
    (is (= {:a 2 :b 3}
           (m/map-vals inc {:a 1 :b 2} {})))
    (is (= {:x 1}
           (m/map-vals inc nil {:x 1}))))
  (testing "map-keys-by-v: new key is (f v), value is preserved"
    (is (= {1 1 2 2}
           (m/map-keys-by-v identity {:a 1 :b 2} {})))
    (is (= {2 1 3 2}
           (m/map-keys-by-v inc {:a 1 :b 2} {}))))
  (testing "map-keys-by-v: collisions overwrite (last wins)"
    (is (= {0 2}
           (m/map-keys-by-v (constantly 0) {:a 1 :b 2} {}))))
  (testing "map-keys-and-vals"
    (is (= {:A 2}
           (m/map-keys-and-vals
            (fn [k v] [(keyword (clojure.string/upper-case (name k))) (inc v)])
            {:a 1} {}))))
  (testing "inversions"
    (is (= {1 #{:a} 2 #{:a :b}}
           (m/map-of-sets-invert {:a #{1 2} :b #{2}})))
    (is (= {1 #{:a} 2 #{:b :c}}
           (m/invert-in-sets {:a 1 :b 2 :c 2})))
    (is (= {1 :a 2 :b}
           (m/map-of-vectors-invert-flatten (array-map :a [1 2] :b [2])))))
  (testing "recursive value mapping"
    (is (= {:a 2 :b {:c 3}}
           (m/map-values inc {:a 1 :b {:c 2}})))
    (is (nil? (m/map-values inc nil)))
    (is (= {:a [1 [:a]] :b {:c [2 [:b :c]]}}
           (m/map-values-with-path (fn [v path] [v path]) {:a 1 :b {:c 2}})))
    (is (= {:a [1 '(:a)] :b {:c [2 '(:c :b)]}}
           (m/map-values-with-rpath (fn [v rpath] [v rpath]) {:a 1 :b {:c 2}})))))

;; ---------------------------------------------------------------------------
;; update-values family
;; ---------------------------------------------------------------------------

(deftest update-values-family
  (testing "update-values does not touch missing keys by default"
    (is (= {:a 2 :b 2}
           (m/update-values {:a 1 :b 2} {:a inc :c inc} false))))
  (testing "update-values can create missing keys"
    (is (= {:a 2 :b 2 :c 1}
           (m/update-values {:a 1 :b 2}
                            {:a inc
                             :c (fn [x] (if (nil? x) 1 (inc x)))}
                            true))))
  (testing "update-values supports remove-key marker"
    (let [rm ::rm]
      (is (= {:b 2}
             (m/update-values {:a 1 :b 2}
                              {:a (fn [_] rm)}
                              false
                              rm)))))
  (testing "update-values-recur touches nested maps/vectors"
    (is (= {:a {:x 2} :b [1 2]}
           (m/update-values-recur {:a {:x 1} :b [1 2]} {:x inc} false))))
  (testing "update-values-or-seqs updates vectors and seqs"
    (is (= {:a [2 3] :b 2}
           (m/update-values-or-seqs {:a [1 2] :b 1} {:a inc :b inc} false)))
    (is (= {:a '(2 3)}
           (m/update-values-or-seqs {:a '(1 2)} {:a inc} false))))
  (testing "update-values-or-seqs-recur recurses into nested vectors"
    (is (= {:a [{:x 2} {:x 3}]}
           (m/update-values-or-seqs-recur {:a [{:x 1} {:x 2}]} {:x inc} false)))))

;; ---------------------------------------------------------------------------
;; misc
;; ---------------------------------------------------------------------------

(deftest dissoc-in-and-utility-functions
  (testing "dissoc-in removes nested keys"
    (is (= {:a {:x 1 :y {}}}
           (m/dissoc-in {:a {:x 1 :y {:z 2}}} [:a :y :z]))))
  (testing "duplicate-keys"
    (is (= {:a 1 :b 2 :aa 1}
           (m/duplicate-keys {:a 1 :b 2} {:a :aa})))
    (is (= {:a 1}
           (m/duplicate-keys {:a 1} {:missing :x}))))
  (testing "nil-keys / nil-existing-keys"
    (is (= {:a nil :b nil}
           (m/nil-keys {:a 1 :b 2} [:a :b])))
    (is (= {:a nil :b 2}
           (m/nil-existing-keys {:a 1 :b 2} [:a :c])))
    (is (nil? (m/nil-keys nil [:a])))))

;; ---------------------------------------------------------------------------
;; Lazy maps
;; ---------------------------------------------------------------------------

(deftest lazy-maps-basic
  (testing "lazy macro does not realize values eagerly"
    (let [calls (atom 0)
          lm    (m/lazy {:a (do (swap! calls inc) 1)
                         :b (do (swap! calls inc) 2)})]
      (is (= 0 @calls))
      (is (= 1 (:a lm)))
      (is (= 1 @calls))
      (is (= 2 (:b lm)))
      (is (= 2 @calls))))
  (testing "to-lazy + lazy?"
    (let [lm (m/to-lazy {:a 1})]
      (is (true? (m/lazy? lm)))
      (is (= 1 (:a lm)))))
  (testing "select-keys-lazy preserves unrealized values"
    (let [calls (atom 0)
          lm    (m/lazy {:a (do (swap! calls inc) 1)
                         :b (do (swap! calls inc) 2)})
          sk    (m/select-keys-lazy lm [:b])]
      (is (= 0 @calls))
      (is (= 2 (:b sk)))
      (is (= 1 @calls))
      (is (nil? (:a sk)))
      (is (= 1 @calls))))
  (testing "merge-lazy returns a lazy map and keeps values lazy"
    (let [calls (atom 0)
          lm1   (m/lazy {:a (do (swap! calls inc) 1)})
          lm2   (m/lazy {:b (do (swap! calls inc) 2)})
          lm    (m/merge-lazy lm1 lm2)]
      (is (m/lazy? lm))
      (is (= 0 @calls))
      (is (= 1 (:a lm)))
      (is (= 1 @calls))
      (is (= 2 (:b lm)))
      (is (= 2 @calls)))))
