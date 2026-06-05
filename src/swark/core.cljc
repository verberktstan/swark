(ns swark.core
  (:require [clojure.string :as str]))

;; SWiss ARmy Knife - Your everyday clojure toolbelt!
;; Copyright 2024-2026 - Stan Verberkt (verberktstan@gmail.com)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Regarding collections

(defn key-by
  {:added "0.1.0"
   :arglists '([f] [f coll] [a f coll])
   :doc "Returns a map of {(f item) item} for each item in coll, omitting falsy keys.
  Returns a transducer when called with f only.
  `(key-by :id [{:id 12} {:id 34}]) => {12 {:id 12} 34 {:id 34}}`"}
  ([f]
   {:pre [(ifn? f)]}
   (comp (map (juxt f identity)) (filter first)))
  ([f coll]
   (key-by {} f coll))
  ([a f coll]
   {:pre [(associative? a)]}
   (into a (key-by f) coll)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Regarding maps

(defn map-vals
  {:added "0.1.0"
   :arglists '([f] [f item])
   :doc "Returns item with f mapped across its values. Returns a transducer
  when called with f only.
  `(map-vals count {:a [:b :c] :d [:e]}) => {:a 2 :d 1}`"}
  ([f]
   {:pre [(ifn? f)]}
   (map (juxt key (comp f val))))
  ([f item]
   {:pre [(ifn? f)]}
   (into {} (map-vals f) item)))

(defn filter-keys
  {:added "0.1.3"
   :arglists '([map pred])
   :doc "Returns a map containing only those entries in map whose key return
   logical true on evaluation of (pred key).
   `(filter-keys {:a 1 \"b\" 2} keyword?) => {:a 1}`"}
  [map pred]
  (reduce-kv
    (fn filter-key* [acc k v]
      (cond-> acc (or (not pred) (pred k)) (assoc k v)))
    {}
    map))

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
  (assert (map? spec) "Spec should be a map!")
  (assert (every? ifn? (vals spec)) "All vals in spec should implement IFn!")
  (some-> input map? (assert "Input should be a map!"))
  (if (nil? input)
    ::nil
    (reduce-kv
      (fn [acc k v]
        (if-let [predicate (get spec k)]
          (let [{{res ::result} :as result} (check predicate v)]
            (cond-> acc (nil? res) (assoc k res)))
          acc))
      nil
      input)))

(def valid-map? (complement invalid-map?))

(defmacro defmemo
  {:added "0.1.52"
   :arglists '([name & fndef] [name memo & fndef])
   :doc "Defines a memoized function and returns it. Accepts an optional memoizer
   as the second argument, defaulting to memoize.
   `(defmemo my-fn [x] (* x x))` uses memoize.
   `(defmemo my-fn memoir [x] (* x x))` uses memoir, gaining :flush capability."}
  [name & args]
  (let [[memo fndef] (if (symbol? (first args))
                       [(first args) (rest args)]
                       [`memoize args])]
    `(let [memoized-fn# (~memo (fn ~@fndef))]
       (def ~name memoized-fn#)
       memoized-fn#)))

(def flush-signal
  "Sentinel value. Pass as the first argument to a memoir fn to trigger cache eviction."
  (Object.))

(defn memoir
  "Like memoize but with flush functionality."
  [f]
  (let [state (atom nil)]
    (fn memoir* [& args]
      (let [flush?     (identical? (first args) flush-signal)
            flush-args (-> args rest seq)]
        (cond
          (and flush? flush-args)
          (let [cache (get @state flush-args)]
            (swap! state dissoc flush-args)
            cache)

          flush?
          (reset! state nil)

          :else
          (if (contains? @state args)
            (get @state args)
            (-> state
                (swap! assoc args (apply f args))
                (get args))))))))
