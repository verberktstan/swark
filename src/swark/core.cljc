(ns swark.core
  (:require [clojure.core.async :as a]
            [clojure.string :as str]))

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
   (comp
    (map (juxt f identity))
    (filter first)))
  ([f coll]
   (when-let [s (seq coll)]
     (-> f ifn? assert)
     (-> s coll? assert)
     (into {} (key-by f) s))))

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
   :doc "Returns the result of (apply f args) after running it n times. When
   something is thrown on the last try, returns the throwable map."}
  [n f & args]
  (-> n pos-int? assert)
  (loop [n n, result nil]
    (if (zero? n)
      (or (jab Throwable->map result) result) ; Try to coerce to map if something is Thrown
      (recur
       (dec n)
       (try
         (apply f args)
         (catch Throwable t t))))))

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
  ([{:keys [min-length] :or {min-length 1}} existing]
   (-> existing set? assert)
   (reduce
    (fn [s char]
      (if (and s (>= (count s) min-length) (-> s existing not) (-> s reverse first #{"-"} not))
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Async stuff

;; TODO: Support channel transducers and ex-handler as well
(defn with-buffer
  {:added "0.1.41"
   :arglist '([x])
   :doc "Starts a go-loop and returns a map with ::in and ::out async channels.
   Input to ::in chan is expected to be [f & args] or [::closed!]. In the latter
   case, the go-loop will stop. In the first case, (apply f x args) will be called
   and the result is put on ::out chan."}
  [x]
  (let [in-chan  (a/chan (a/sliding-buffer 99))
        out-chan (a/chan (a/dropping-buffer 99))]
    (a/go-loop [[f & args] (a/<! in-chan)]
      (when-not (some-> f #{::closed!}) ; NOTE: Stop the go-loop in this case
        (if-let [result (when f (apply f x args))]
          (a/>! out-chan result)
          (a/>! out-chan ::nil))
        (recur (a/<! in-chan))))
    {::in  in-chan
     ::out out-chan}))

(defn put!
  {:added "0.1.41"
   :arglist '([buffered & args])
   :doc "Put args on the ::in chan and blocks until something is returned via
   ::out chan. Returns the returned value."}
  [{::keys [in out]} & args]
  (assert in)
  (assert out)
  (a/go (a/>! in (or args [::closed!]))) ; NOTE: Close the go-loop when nil args
  (a/<!! out))

(defn close!
  {:added "0.1.41"
   :arglist '([buffered])
   :doc "Stops the underlying go-loop and closes all channels. Returns nil."}
  [{::keys [in] :as buffered}]
  (assert in)
  (a/>!! in [::closed!]) ; NOTE: Close the running go-loop
  (let [channels (juxt ::in ::out)]
    (run! a/close! (channels buffered))))
