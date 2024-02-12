(ns swark.authom
  {:added "0.1.3"
   :doc "(Re)store auth related stuff in clojure metadata."}
  (:require [swark.core :refer [jab]]))

;; swark.authom - Atomic authorisation made easy

(defn- ->hash
  "Returns the hash code of `item`. Tries to find it for collections if standard hashing fails."
  ([item]
   (assert item)
   (or (jab hash item)
       (jab hash-unordered-coll item)
       (jab hash-ordered-coll item)
       (jab mix-collection-hash item 2)))
  ([item pass]
   (assert pass)
   (->hash {::item item ::pass pass}))
  ([item pass secret]
   (assert secret)
   (->hash {::item item ::secret secret} pass)))

(defn- restore-meta-token*
  [item token]
  (vary-meta item assoc ::token token))

(defn with-meta-token
  "Returns the item with the hashed token in it's metadata. `item` should implement IMeta, otherwise this simply returns nil."
  [item & [pass secret :as args]]
  (try
    (restore-meta-token* item (str (apply ->hash item args)))
    #?(:cljs (catch :default nil)
       :clj (catch Throwable _ nil))))

(defn map-with-meta-token
  "Returns the map `m` with the hashed token in it's metadata. Only accepts a map and primary-key must be present in map `m`."
  [m primary-key & [pass secret :as args]]
  (-> m map? assert)
  (-> m (get primary-key) assert)
  (merge (apply with-meta-token (select-keys m [primary-key]) args) m))

;; Simply return the token from the Authom metadata
(def meta-token (comp ::token meta))

(defn check-meta-token
  "Returns the item when pass and secret are valid with respect to the item.
  Throws an AssertionError if `item` doens't have a meta-token."
  [item & [pass secret :as args]]
  (let [token (meta-token item)]
    (assert token)
    (when (= token (str (apply ->hash item args)))
      item)))

(defn map-check-meta-token
  [m primary-key & [pass secret :as args]]
  (-> m map? assert)
  (-> m (get primary-key) assert)
  (apply check-meta-token (select-keys m [primary-key]) args))

(defn enrich-token
  "Returns map with Authom's meta-token associated with ::token."
  [map]
  (-> map map? assert)
  (let [token (meta-token map)]
    (cond-> map
      token (assoc ::token token))))

(defn restore-enriched-token
  "Returns map with the value in ::token restored as meta-token."
  [{::keys [token] :as map}]
  (-> map map? assert)
  (cond-> map
    token (restore-meta-token* token)
    token (dissoc ::token)))

;; NOTE: Default props to make swark.cedric serialize and parse Authom tokens automatically
(def CEDRIC-PROPS
  {:pre-upsert-serializer enrich-token
   :post-merge-parser restore-enriched-token})
