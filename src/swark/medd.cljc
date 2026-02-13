(ns swark.medd
  (:require [clojure.data :refer [diff]]
            [clojure.test :as t]))

;; Minimalistic event driven database

;; Upsert

(def ->entity (juxt :entity-attribute :entity-value))

(defn- combine-reducer
  [m {:keys [entity-attribute entity-value attribute value]}]
  (let [entity [entity-attribute entity-value]]
    (assert entity)
    (update m entity assoc attribute value entity-attribute entity-value)))

(defn- combine-rows
  ([db-rows]
   (combine-rows nil db-rows))
  ([entity db-rows]
   (reduce
     combine-reducer
    {}
    (into [] (cond->> db-rows entity (filter (comp #{entity} ->entity)))))))

(t/deftest combine-rows-test
  (t/testing "Combines rows into a record"
    (t/is (= {[:user/id 1] {:user/id 1 :user/name "User Name"}}
             (combine-rows
              [:user/id 1]
              [{:entity-attribute :user/id :entity-value 1 :attribute :user/name :value "User Name"}])))
    (t/is (= {[:user/id 1] {:user/id 1 :user/name "User Name"}
              [:user/id 2] {:user/id 2 :user/name "Second User"}}
             (combine-rows
              [{:entity-attribute :user/id :entity-value 1 :attribute :user/name :value "User Name"}
               {:entity-attribute :user/id :entity-value 2 :attribute :user/name :value "Second User"}])))))

(defn upserted-rows
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

(t/deftest upserted-rows-test
  (t/testing "Returns a row for every attribute/value pair"
    (t/is (= [{:entity-attribute :user/id :entity-value 1 :attribute :user/name :value "User Name"}]
             (upserted-rows :user/id {:user/id 1 :user/name "User Name"})))
    (t/is (= #{{:entity-attribute :user/id :entity-value 1 :attribute :user/city :value "The City"}
               {:entity-attribute :user/id :entity-value 1 :attribute :user/name :value "User Name"}}
             (set (upserted-rows :user/id {:user/id 1 :user/name "User Name" :user/city "The City"}))))
    (t/is (nil? (upserted-rows :user/id {:user/id 1})))
    (t/is (thrown-with-msg? Throwable #"Assert failed" (upserted-rows :user/id nil)))
    (t/is (thrown-with-msg? Throwable #"Assert failed" (upserted-rows :user/id {:user/name "User Name"})))
    (t/testing "ignoring rows that are already present in db-rows"
      (t/is (= [{:entity-attribute :user/id :entity-value 1 :attribute :user/name :value "User Name"}]
               (let [rows [{:entity-attribute :user/id :entity-value 1 :attribute :user/city :value "The City"}]]
                 (upserted-rows :user/id {:user/id 1 :user/name "User Name" :user/city "The City"} rows)))))))

;; Read
;; Archive
