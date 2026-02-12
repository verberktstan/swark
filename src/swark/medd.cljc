(ns swark.medd
  (:require [clojure.test :as t]))

;; Minimalistic event driven database

;; Upsert

(defn upserted-rows
  [entity-key record]
  (-> record map? assert)
  (let [entity (find record entity-key)]
    (assert entity)
    (reduce-kv
     (fn [rows attribute value]
       (conj rows {:entity entity :attribute attribute :value value}))
     nil
     (dissoc record entity-key))))

(t/deftest upserted-rows-test
  (t/testing "Returns a row for every attribute/value pair"
    (t/is (= [{:entity [:user/id 1] :attribute :user/name :value "User Name"}]
             (upserted-rows :user/id {:user/id 1 :user/name "User Name"})))
    (t/is (= #{{:entity [:user/id 1] :attribute :user/city :value "The City"}
               {:entity [:user/id 1] :attribute :user/name :value "User Name"}}
             (set (upserted-rows :user/id {:user/id 1 :user/name "User Name" :user/city "The City"}))))
    (t/is (thrown-with-msg? Throwable #"Assert failed" (upserted-rows :user/id nil)))
    (t/is (thrown-with-msg? Throwable #"Assert failed" (upserted-rows :user/id {:user/name "User Name"})))))

;; Read
;; Archive
