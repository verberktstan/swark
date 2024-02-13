(ns swark.authom
  {:added "0.1.3"
   :doc "(Re)store auth related stuff in clojure metadata."}
  (:require [swark.core :refer [->str jab]]))

;; swark.authom - Atomic authorisation made easy

(defn- ->hash
  {:added "0.1.4"
   :arglist '([item] [item pass] [item pass secret])
   :doc "Returns the hash code of item. Tries to find it for collections if standard hashing fails."}
  ([item]
   (assert item)
   (->str
    (or (jab hash item)
        (jab hash-unordered-coll item)
        (jab hash-ordered-coll item)
        (jab mix-collection-hash item 2))))
  ([item pass]
   (assert pass)
   (->hash {::item item ::pass pass}))
  ([item pass secret]
   (assert secret)
   (->hash {::item item ::secret secret} pass)))

(defn- restore
  {:added "0.1.4"
   :arglist '([map token])
   :doc "Returns map with token stored in it's metadata."}
  [map token]
  (vary-meta map assoc ::token token))

(defn with-token
  {:added "0.1.4"
   :arglist '([map key pass] [map key pass secret])
   :doc "Returns map with the hashed token in it's metadata. Only accepts a map and primary-key must be present in map."}
  ([map key pass]
   (with-token map key pass nil))
  ([map key pass secret]
   (-> map map? assert)
   (-> map (get key) assert)
   (letfn [(with-token* [map & args] (jab restore map (apply ->hash map args)))]
     (merge
      (-> map
          (select-keys [key])
          (with-token* pass secret))
      map))))

(def token
  ^{:added "0.1.4"
    :arglist '([map])
    :doc "Returns the token from map's metadata."}
  (comp ::token meta))

(defn check
  {:added "0.1.4"
   :arglist '([map primary-key pass] [map primary-key pass secret])
   :doc "Returns map when token check is successful, else returns nil."}
  ([map primary-key pass]
   (check map primary-key pass nil))
  ([map primary-key pass secret]
   (-> map token boolean assert)
   (when (-> map token (= (-> map (with-token primary-key pass secret) token)))
     map)))

(defn disclose
  {:added "0.1.4"
   :arglist '([map] [key map])
   :doc "Returns map with Authom's meta-token associated with ::token."}
  ([map]
   (disclose nil map))
  ([key map]
   (-> map map? assert)
   (let [token (token map)]
     (cond-> map
       token (assoc (or key ::token) token)))))

(defn conceal
  {:added "0.1.4"
   :arglist '([map] [key map])
   :doc "Returns map with the value in ::token concealed as meta-token."}
  ([map]
   (conceal nil map))
  ([key {::keys [token] :as map}]
   (-> map map? assert)
   (cond-> map
     token (restore token)
     token (dissoc (or key ::token)))))

;; NOTE: Default props to make swark.cedric serialize and parse Authom tokens automatically
(def CEDRIC-PROPS
  {:pre-upsert-serializer disclose
   :post-merge-parser conceal})
