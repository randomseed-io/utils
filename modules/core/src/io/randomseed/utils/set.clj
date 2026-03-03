(ns

    ^{:doc    "Random Utilities, set support functions."
      :author "Paweł Wilk"
      :added  "1.0.0"}

    io.randomseed.utils.set

  (:require [clojure.set :as set]))

(defn replace-in
  "Replaces `old-val` with `new-val` in set `s`."
  [^clojure.lang.PersistentHashSet s old-val new-val]
  (conj (disj s old-val) new-val))

(defn difference
  "Like `clojure.set/difference` but optimized with transients and supports an
  optional transducer `xform`."
  ([] [])
  ([of] of)
  ([of what]
   (if (>= (count of) (count what))
     (if (instance? clojure.lang.IEditableCollection of)
       (with-meta (persistent! (reduce disj! (transient of) what)) (meta of))
       (reduce disj of what))
     (if (instance? clojure.lang.IEditableCollection of)
       (with-meta
         (persistent!
          (reduce (fn [result item]
                    (if (contains? what item)
                      (disj! result item)
                      result))
                  (transient of) of))
         (meta of))
       (reduce (fn [result item]
                 (if (contains? what item)
                   (disj result item)
                   result))
               of of))))
  ([of xform what]
   (if (>= (count of) (count what))
     (if (instance? clojure.lang.IEditableCollection of)
       (with-meta (persistent! (transduce xform disj! (transient of) what)) (meta of))
       (transduce xform disj of what))
     (if (instance? clojure.lang.IEditableCollection of)
       (with-meta
         (persistent!
          (transduce xform
                     (fn [result item]
                       (if (contains? what item)
                         (disj! result item)
                         result))
                     (transient of) of))
         (meta of))))))

(defn out-of
  "Removes elements of `what` from set `of`. Uses `difference` when both are sets,
  otherwise falls back to transient-based `disj`."
  ([] [])
  ([of] of)
  ([of what]
   (if (and (set? what) (set? of))
     (difference of what)
     (if (instance? clojure.lang.IEditableCollection of)
       (with-meta (persistent! (reduce disj! (transient of) what)) (meta of))
       (reduce disj of what))))
  ([of xform what]
   (if (and (set? what) (set? of))
     (difference of xform what)
     (if (instance? clojure.lang.IEditableCollection of)
       (with-meta (persistent! (transduce xform disj! (transient of) what)) (meta of))
       (transduce xform disj of what)))))
