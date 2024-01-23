(ns swark.authom
  (:require [swark.core :refer [try?]]))

(defn- ->hash
  "Returns the hash code of `item`. Tries to find it for collections if standard hashing fails."
  ([item]
   (assert item)
   (or (try? hash item)
       (try? hash-unordered-coll item)
       (try? hash-ordered-coll item)
       (try? mix-collection-hash item 2)))
  ([item pass]
   (assert pass)
   (->hash {::item item ::pass pass}))
  ([item pass secret]
   (assert secret)
   (->hash {::item item ::secret secret} pass)))

(comment
  ;; TODO: Turn fiddle code into tests
  (hash 1)
  (hash-unordered-coll 1)
  (->hash {:user/id 123})
  (->hash {:user/id 123} "password")
  (->hash {:user/id 123} "password" "SECRET")
  )

(defn with-meta-token
  "Returns the item with the hashed token in it's metadata. `item` should implement IMeta, otherwise this simply returns nil."
  [item & [pass secret :as args]]
  (try
    (vary-meta item assoc ::token (apply ->hash item args))
    #?(:cljs (catch :default nil)
       :clj (catch Throwable _ nil))))

(comment
  ;; TODO: Turn fiddle code into tests
  (-> {:user/id 123} with-meta-token meta)
  (-> {:user/id 123} (with-meta-token "password") meta)
  (-> {:user/id 123} (with-meta-token "password" "SECRET") meta)
  )

(def meta-token (comp ::token meta))

(defn check-meta-token
  "Returns the item when pass and secret are valid with respect to the item.
  Throws an AssertionError if `item` doens't have a meta-token."
  [item & [pass secret :as args]]
  (let [token (meta-token item)]
    (assert token)
    (when (= token (apply ->hash item args))
      item)))

(comment
  ;; TODO: Turn fiddle code into tests
  (let [user (with-meta-token {:user/id 123} "password" "SECRET")]
    {:valid   (check-meta-token user "password" "SECRET")
     :invalid (check-meta-token user "wrong-password" "SECRET")})

  ;; Example usage with SQL database via jdbc
  (let [primary-key  :user/id
        user         {:user/id 123 :user/name "User Name"}
        user'        (-> user
                         (select-keys [primary-key]) ; Generate meta token only with primary map-entry
                         (with-meta-token "pass")
                         (merge user))
        get-rows     (juxt :user/id meta-token :user/name) ; Retrieve id, token and name from user record
        rows         (get-rows (merge user' user)) ; NOTE: The metadata is preserved from user'
        upsert-query (into ["REPLACE INTO users(id,token,name) values(?,?,?)"] rows)] ; Construct a SQL query to upsert user rows in the database.
    (with-open [connection (-> {:dbname "test"} jdbc/get-datasource jdbc/get-connection)]
      (jdbc/execute! connection upsert-query)))

  ;; Example usage - update user in appdb
  (let [primary-key :user/id
        user        {:user/id 123 :user/name "User Name"}
        primary-val (get user primary-key)
        user'       (-> user
                        (select-keys [primary-key]) ; Generate meta token only with primary map-entry
                        (with-meta-token "pass")
                        (merge user))
        db          {}]
    (-> db
        (update-in [:users primary-val] (partial merge user')) ; Update user in db
        :users
        (get primary-val)
        meta-token))
  )
