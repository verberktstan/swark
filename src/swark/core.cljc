(ns swark.core
  (:require [clojure.string :as str]))

;; SWiss ARmy Knife - Your everyday clojure toolbelt!
;; Copyright 2024 - Stan Verberkt (verberktstan@gmail.com)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Regarding collections

(defn key-by
 {:added "0.1.0"
  :arglist '([f coll])
  :doc "Returns a map containing all items in coll, associated by the return
  value of (f val). When the key is logical false, it is not included in
  the returned map. Returns a transducer when no collection is provided.
  `(key-by :id [{:id 12} {:id 34}]) => {12 {:id 12} 34 {:id 34}}`"}
  ([f]
    (-> f ifn? assert)
    (comp (map (juxt f identity)) (filter first)))
  ([f coll]
    (when-let [s (seq coll)]
      (key-by {} f coll)))
  ([a f coll]
    (-> a associative? assert)
    (cond-> a (seq coll)
      (into (key-by f) coll))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Regarding maps

(defn map-vals
  {:added "0.1.0"
   :arglist '([f item])
   :doc "Returns item with f mapped across it's values. Returns a transducer
        when no collection is provided.
        `(map-vals count {:a [:b c] :d [:e]}) => {:a 2 :d 1}`"}
  ([f]
   (map (juxt key (comp f val))))
  ([f item]
   (when item
     (-> f ifn? assert)
     (-> item map? assert)
     (into {} (map-vals f) item))))

(defn filter-keys
  {:added "0.1.3"
   :arglists '([map pred])
   :doc "Returns a map containing only those entries in map whose key return
   logical true on evaluation of (pred key).
   `(filter-keys {:a 1 \"b\" 2} keyword?) => {:a 1}`"}
  [map pred]
  (cond->> map
    pred    (filter (comp pred key))
    map     seq
    :always (into {})))

(declare jab)

(defn select-namespaced
  {:added "0.1.3"
   :arglist '([map] [map ns])
   :doc "Returns a map containing only those entries in map whose keys'
   namespace match ns. When ns is nil, returns a map containing only
   non-namespaced keys.
   `(select-namespaced {::test 1 :test 2} (namespace ::this)) => {::test 1}`"}
  ([map]
   (select-namespaced map nil))
  ([map ns]
   (-> map map? assert)
   (let [ns (jab name ns)
         predicate (if ns #{ns} nil?)]
     (filter-keys map (comp predicate namespace)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Try and catch

(defn jab
  {:added "0.1.3"
   :arglists '([f & args])
   :doc "Returns the result of (apply f args). When any error or exception is
   thrown, simply returns nil instead. So jab is like try but it fails silently.
   `(jab inc nil) => nil`"}
  [f & args]
  (try
    (apply f args)
    #?(:cljs (catch :default _ nil) :clj (catch Throwable _ nil))))

;; TODO: Add tests
(defn with-retries
  {:added "0.1.41"
   :arglist '([n f & args])
   :doc "Returns the result of (apply f args) after retrying up to n times. When
   something is thrown on the last try, returns the throwable map."}
  [n f & args]
  (-> n pos-int? assert)
  (loop [retries-left n]
    (let [result (if (zero? retries-left)
                   (try
                     (apply f args)
                     (catch
                         #?(:cljs :default :clj Throwable)
                         t
                       #?(:cljs t :clj (Throwable->map t))))
                   (apply jab f args))]
      (cond
        (zero? retries-left) {:throwable result :retries-left retries-left :n n}
        result {:result result :retries-left retries-left :n n}
        :else (recur (dec retries-left))))))

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
     (let [stringify (if (jab name input) name str)]
       (some-> input stringify str/trim non-blank)))))

(defn unid
  "Returns a unique string that does is not yet contained in the existing set."
  ([] (-> (random-uuid) str))
  ([existing]
   (unid nil existing))
  ([{:keys [min-length filter-regex no-dashes?] :or {min-length 1}} existing]
   ;; (-> existing set? assert)
   (assert (or (map? existing) (set? existing)))
   (reduce
    (fn [s char]
      (if (and s (>= (count s) min-length) (->> s (contains? existing) not) (-> s reverse first #{"-"} not))
        (reduced s)
        (str s char)))
    nil
    (cond->> (seq (unid))
      no-dashes?   (remove #{\-})
      filter-regex (filter (comp (partial re-find filter-regex) str))))))

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
   `(valid-map? {:a string?} {:a 12}) ≠> {::predicate string? ::input 12 ::result false}`"}
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
          (let [cache (get @state flush-args)]
            (swap! state dissoc flush-args)
            cache)

          flush?
          (reset! state nil)

          :else
          (or (get @state args)
              (-> state
                  (swap! assoc args (apply f args))
                  (get args))))))))


