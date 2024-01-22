(ns swark.core
  (:require [clojure.string :as str]))

;; SWiss ARmy Knife - Your everyday clojure toolbelt!
;; Copyright 2024 - Stan Verberkt (verberktstan@gmail.com)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Regarding collections

(defn key-by
  "Returns a map with all items in collection `coll` keyed by the value returned by `(f item)`
  When `(f item)` returns a falsey value, it is NOT included in the resulting map."
  [f coll]
  (when coll
    (-> f ifn? assert)
    (-> coll coll? assert)
    (->> coll
         (map (juxt f identity))
         (filter first)
         (into {}))))

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Regarding strings

(defn ->string
  "Returns `input` coerced to a trimmed string. Returns nil instead of a blank string."
  [input]
  (letfn [(non-blank [s] (when-not (str/blank? s) s))]
    (when input
      (some-> input name str/trim non-blank))))

(comment
  (->string "Hello, Swark!") ; "Hello, Swark!"
  (->string :keyword2) ; "keyword2"
  (->string 'symbol3) ; "symbol3"
  (->string "     ") ; nil
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Regarding keywords

(defn ->keyword
  "Coerces `input` to a keyword, replacing whitespace with dashes by default."
  ([input]
   (->keyword #"\s" input))
  ([ignore-match input]
   (->keyword ignore-match "-" input))
  ([ignore-match replace-with input]
   (when input
     (some-> input name str/trim str/lower-case (str/replace ignore-match replace-with) keyword))))

(comment
  (->keyword :test)
  (->keyword "hello")
  (->keyword " H ell-oo1")
  ;; TODO: Support namespaced keywords :-)
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
