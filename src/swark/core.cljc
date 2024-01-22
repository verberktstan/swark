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

(defn ->str
  "Returns `input` coerced to a trimmed string. Returns nil instead of a blank string."
  [input]
  (letfn [(non-blank [s] (when-not (str/blank? s) s))]
    (when input
      (some-> input name str/trim non-blank))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Regarding keywords

;; TODO: Support namespaced keywords :-)
(defn ->keyword
  "Coerces `input` to a keyword, replacing whitespace with dashes by default."
  ([input]
   (->keyword nil input))
  ([ignore-match input]
   (->keyword ignore-match "-" input))
  ([ignore-match replacement input]
   (let [match        (or ignore-match #"\s")
         replacement' (or replacement "-")]
     (when input
       (some-> input name str/trim str/lower-case (str/replace match replacement') keyword)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Minimalistic spec

(defn invalid?
  "Returns nil if map `item` is valid according to map `spec`.
  When `item` is nil, returns ::nil.
  When invalid? returns a set of keys from `item` that don't respect `spec`."
  [spec item]
  (-> spec map? (assert "Spec should be a map!"))
  (assert (->> spec vals (every? ifn?)) "All vals in spec should implement IFn!")
  (some-> item map? (assert "Item should be a map!"))
  (case item
    nil ::nil ; Explicit inform that item is nil
    (some->> item
             (merge-with (fn [predicate value] (predicate value)) spec)
             (remove val)
             seq
             (map first)
             set)))

(def valid? (complement invalid?))
