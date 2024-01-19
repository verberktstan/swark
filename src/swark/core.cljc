(ns swark.core)

;; SWiss ARmy Knife - Your everyday clojure toolbelt!
;; Copyright 2024 - Stan Verberkt (verberktstan@gmail.com)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Regarding collections

(defn key-by
  "Returns a map with all items in collection `coll` keyed by the value returned by `(f item)`"
  [f coll]
  (when coll
    (-> f ifn? assert)
    (-> coll coll? assert)
    (->> coll
         (map (juxt f identity))
         (into {}))))

(comment
  (key-by :id [{:id 0 :name "ab"} {:id 1 :name "cd"}])
  (key-by nil [{:id 0 :name "ab"} {:id 1 :name "cd"}])
  (key-by :id nil)
  (key-by (partial reduce +) [[1 1] [1 2] [2 3 5] [3 5 8 13]])
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Regarding maps

(defn map-vals
  "Returns map `m` with function `f` applied to all its values."
  [f m]
  (when m
    (-> f ifn? assert)
    (-> m map? assert)
    (->> m
         (map (juxt key (comp f val)))
         (into {}))))

(comment
  (map-vals inc {:a 1 :b 2 :c 3})
  (map-vals nil {:a 1 :b 2 :c 3})
  (map-vals inc nil)
  (map-vals (partial reduce +) {:a [1 1] :b [1 2] :c [2 3 5] :d [3 5 8 13]})
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Minimalistic spec

(defn invalid?
  "Returns nil if map `item` is valid according to map `spec`.
  When `item` is nil, returns ::nil.
  When invalid? returns a set of keys from `item` that don't respect `spec`."
  [spec item]
  (-> spec map? assert)
  (->> spec vals (every? ifn?) assert)
  (some-> item map? assert)
  (case item
    nil ::nil
    (some->> item
             (merge-with (fn [predicate value] (predicate value)) spec)
             (remove val)
             seq
             (map first)
             set)))

(def valid? (complement invalid?))

(comment
  (invalid? {:id nat-int?} {:id 0})
  (valid? {:id nat-int?} {:id 0})

  (invalid? {:id nat-int?} {:id -1})
  (valid? {:id nat-int?} {:id -1})

  (invalid? nil {:id -1})
  (valid? nil {:id -1})

  (invalid? {} nil)
  (valid? {} nil)

  (invalid? {:id nat-int?} nil)
  (valid? {:id nat-int?} nil)

  (invalid? {:id nat-int?} false)
  (valid? {:id nat-int?} false)
  )
