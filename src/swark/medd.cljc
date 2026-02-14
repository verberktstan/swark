(ns swark.medd
  (:require [clojure.data :refer [diff]]))

;; Minimalistic event driven database

;; Upsert

(def ->entity (juxt :entity-attribute :entity-value))

(defn- combine-reducer
  [m {:keys [entity-attribute entity-value attribute value]}]
  (let [entity [entity-attribute entity-value]]
    (->> entity (every? some?) assert)
    (update m entity assoc attribute value entity-attribute entity-value)))

(defn- combine-rows
  ([db-rows]
   (combine-rows nil db-rows))
  ([entity db-rows]
   (reduce
     combine-reducer
    {}
    (into [] (cond->> db-rows entity (filter (comp #{entity} ->entity)))))))

(defn- upserted-rows
  ([entity-key record]
   (-> record map? assert)
   (let [[entity-attribute entity-value :as entity] (find record entity-key)]
     (assert entity)
     (reduce-kv
      (fn [rows attribute value]
        (conj rows {:entity-attribute entity-attribute
                    :entity-value     entity-value
                    :attribute        attribute
                    :value            value}))
      nil
      (dissoc record entity-key))))
  ([entity-key record db-rows]
   (let [entity (find record entity-key)]
     (assert entity)
     (let [existing-record (get (combine-rows entity db-rows) entity)
           difff           (zipmap [:a :b :overlap] (diff existing-record record))
           record'         (reduce dissoc record (some-> difff :overlap keys))]
       (upserted-rows entity-key (into record' [entity]))))))

;; Read
;; Archive
