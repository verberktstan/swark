(ns swark.core
  (:require [clojure.string :as str]))

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
  (when item
    (-> f ifn? assert)
    (-> item map? assert)
    (->> item
         (map (juxt key (comp f val)))
         (into {}))))

(defn filter-keys
  {:added "0.1.3"
   :arglists '([map pred])
   :doc "Returns a map containing only those entries in map whose key return
   logical true on evaluation of (pred key).
   `(filter-keys {:a 1 \"b\" 2} keyword?) => {:a 1}`"
   :static true}
  [map pred]
  (cond->> map
    pred    (filter (comp pred key))
    map     seq
    :always (into {})))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Try and return nil when something is thrown

(defn try?
  {:added "0.1.3"
   :arglists '([f & args])
   :doc "Returns the result of (apply f args). When any error or exception is
   thrown, simply returns nil instead.
   `(try? inc nil) => nil`"
   :static true}
  [f & args]
  (try
    (apply f args)
    #?(:cljs (catch :default _ nil) :clj (catch Throwable _ nil))))

(defn select-namespaced
  {:added "0.1.3"
   :arglist '([map] [map ns])
   :doc "Returns a map containing only those entries in map whose keys'
   namespace match ns. When ns is nil, returns a map containing only
   non-namespaced keys.
   `(select-namespaced {::test 1 :test 2} (namespace ::this)) => {::test 1}`"
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
      (if (try? name input)
        (some-> input name str/trim non-blank)
        (some-> input str/trim non-blank)))))

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
   (if (keyword? input)
     input
     (let [match        (or ignore-match #"\s")
           replacement' (or replacement "-")]
       (when input
         (some-> input name str/trim str/lower-case (str/replace match replacement') keyword))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Minimalistic spec


(defn- check [predicate input]
  {::predicate predicate ::input input ::result (predicate input)})

(defn invalid-map?
 {:added "0.1.1"
   :arglist '([spec input])
   :doc "Returns nil if input is valid according to spec. When input is invalid,
   returns a map reporting how it is invalid. When input is nil, returns the
   special keyword ::nil.
   `(valid-map? {:a string?} {:a 12}) ≠> {::predicate string? ::input 12 ::result false}`"
   :static true}
  [spec input]
  (-> spec map? (assert "Spec should be a map!"))
  (assert (->> spec vals (every? ifn?)) "All vals in spec should implement IFn!")
  (some-> input map? (assert "Input should be a map!"))
  (if (nil? input)
    ::nil ; Explicit inform that input is nil
    (some->> input
      (merge-with check spec)
      (remove (comp ::result val))
      seq
      (into {}))))

(def valid-map? (complement invalid-map?))

(defn memoir
  "Like memoize but with flush functionality."
  [f]
  (let [state (atom nil)]
    (fn memoir* [& args]
      (let [flush?     (-> args first #{:flush})
            flush-args (-> args rest seq)]
        (cond
          (and flush? flush-args)
          (swap! state dissoc flush-args)

          flush?
          (reset! state nil)

          :else
          (or (get @state args)
              (-> state
                  (swap! assoc args (apply f args))
                  (get args))))))))

(comment
  (def summ (memoir (partial reduce + 0)))
  (summ [1 3 5]) ; Return 9 and cache the value for this input
  (summ [9 11 13]) ; Return 33 (and cache)
  (summ :flush [9 11 13]) ; Flush the cache for one input
  (summ :flush [1 3 5])

  (summ [1 3])
  (summ [4 6])
  (summ [10 12])
  (summ :flush) ; Flush the complete cache (for all inputs)
  )