(ns swark.core
  (:require [clojure.string :as str])
  (:import [clojure.lang Named]))

;; SWiss ARmy Knife - Your everyday clojure toolbelt!
;; Copyright 2024 - Stan Verberkt (verberktstan@gmail.com)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Regarding collections

(defn key-by
  {:added "0.1.0"
   :arglist '([f coll])
   :doc "Returns a map containing (all) items in coll, associated by the
   return value of (f val). When the key is logical false, it is not included in
   the returned map.
   `(key-by count [[:a] [:b :c]]) => {1 [:a] 2 [:b :c]}`"
   :static true}
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
  {:added "0.1.0"
   :arglist '([f item])
   :doc "Returns item with f mapped across it's values.
   `(map-vals inc {:a 1}) => {:a 2}`"
   :static true}
  [f item]
  (when m
    (-> f ifn? assert)
    (-> item map? assert)
    (->> item
         (map (juxt key (comp f val)))
         (into {}))))

(defn filter-keys
  [map pred]
  {:added "0.1.3"
   :arglists '([map pred])
   :doc "Returns a map containing only those entries in map whose key return logical true on evaluation of (pred key)."
   :static true}
  (cond->> map
    pred    (filter (comp pred key))
    map     seq
    :always (into {})))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Try and return nil when something is thrown

(defn try?
  "Returns the result of (apply f args). When any error or exception is trown,
  returns `nil`."
  [f & args]
  (try
    (apply f args)
    #?(:cljs (catch :default _ nil) :clj (catch Throwable _ nil))))

(defn select-namespaced
  {:added "0.1.3"
   :arglist '([map] [map ns])
   :doc "Returns a map containing only those entries in map whose keys' namespace match ns. When ns is nil, returns a map containing only non-namespaced keys."
   :static true}
  ([map]
   (select-namespaced map nil))
  ([map ns]
   (-> map map? assert)
   (let [ns (try? name ns)
         predicate (if ns #{ns} nil?)]
     (filter-keys map (comp predicate namespace)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Regarding strings

(defn ->str
  "Returns `input` coerced to a trimmed string. Returns nil instead of a blank string. Returns 'namespace/name' for a namespaced keyword."
  [input]
  (letfn [(non-blank [s] (when-not (str/blank? s) s))]
    (or
      (when (keyword? input)
        (->> ((juxt namespace name) input)
          (keep identity)
          (map ->str)
          (str/join "/")))
      (when input
        (some-> input name str/trim non-blank)))))

(defn unid
  "Returns a unique string that does is not yet contained in the existing set."
  ([] (-> (random-uuid) str))
  ([existing]
    (-> existing set? assert) 
    (reduce
      (fn [s char]
        (if (and s (-> s existing not) (-> s reverse first #{"-"} not))
          (reduced s)
          (str s char)))
      nil
      (seq (unid)))))

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

(defn invalid-map?
  "Returns nil if map `input` is valid according to map `spec`.
  When `input` is nil, returns {::input nil}.
  When `input` is invalid, returns a map with a report on why it is invalid."
  [spec input]
  (-> spec map? (assert "Spec should be a map!"))
  (assert (->> spec vals (every? ifn?)) "All vals in spec should implement IFn!")
  (some-> input map? (assert "Input should be a map!"))
  (case input
    nil {::input input} ; Explicit inform that input is nil
    (some->> input
             (merge-with (fn [predicate input] {::predicate predicate ::input input ::result (predicate input)}) spec)
             (remove (comp ::result val))
             seq
             (into {}))))

(def valid-map? (complement invalid-map?))