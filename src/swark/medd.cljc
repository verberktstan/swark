(ns swark.medd
  (:require [clojure.data :refer [diff]]))

;; Minimalistic event driven database

;; Regarding rows (represented as a map)
(defn row? [m] (every? m [:entity-attribute :entity-value :attribute :value]))
(def ->entity (juxt :entity-attribute :entity-value))
(def ->attval (juxt :attribute :value))
(def ->record (comp (partial apply assoc {}) ->attval))

;; Upsert

;; TODO: Filter based on attribute/value as well?
(defmacro filter-xform
  [entity-attribute-pred entity-value-pred entity-pred]
  (let [no-entity-pred? (not entity-pred)
        ea-filterer     (when (and no-entity-pred? entity-attribute-pred)
                          `(filter
                            (comp ~entity-attribute-pred :entity-attribute)))
        ev-filterer     (when (and no-entity-pred?) entity-value-pred
                          `(filter
                            (comp ~entity-value-pred :entity-value)))
        entity-filterer (when entity-attribute-pred
                          `(filter
                            (comp ~entity-pred (juxt :entity-attribute :entity-value))))]
    (cons `comp (keep identity [ea-filterer ev-filterer entity-filterer]))))

(defn combine
  "Returns map m with row data merged into the value associated with the row's entity."
  ([] {})
  ([m] m)
  ([m row]
   (-> m map? assert)
   (-> row row? assert)
   (update m (->entity row) merge (->record row))))

(defn combine-rows
  [{:keys [entity-attribute-pred entity-value-pred entity-pred]} db-rows]
  (transduce
    (filter-xform entity-attribute-pred entity-value-pred entity-pred)
    combine
    db-rows))

;; TODO: Support multiple records in one go?
(defn- upserted-rows
  ([entity-key record]
   (-> record map? assert)
   (let [[ea ev :as entity] (find record entity-key)]
     (assert entity)
     (reduce-kv
      (fn [rows attribute value]
        (conj rows {:entity-attribute ea
                    :entity-value     ev
                    :attribute        attribute
                    :value            value}))
      nil
      (dissoc record entity-key))))
  ([entity-key record db-rows]
   (let [[ea ev :as entity] (find record entity-key)]
     (assert entity)
     (let [props           {:entity-attribute-pred #{ea}
                            :entity-value-pred     #{ev}
                            :entity-pred #{entity}}
           existing-record (get (combine-rows props db-rows) entity)
           [_ _ overlap]   (diff existing-record record)]
       (upserted-rows
        entity-key
        (into (reduce dissoc record (keys overlap)) [entity]))))))

;; Read
;; Archive
