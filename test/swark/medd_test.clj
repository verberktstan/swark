(ns swark.medd-test
  (:require [clojure.test :as t]
            [clojure.test.check.clojure-test :refer [defspec]]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]
            [swark.medd :as sut]))

(t/deftest combine-rows-test
  (t/testing "Combines rows into a record"
    (t/is (= {[:user/id 1] {:user/id 1 :user/name "User Name"}}
             (#'sut/combine-rows
              [:user/id 1]
              [{:entity-attribute :user/id :entity-value 1 :attribute :user/name :value "User Name"}])))
    (t/is (= {[:user/id 1] {:user/id 1 :user/name "User Name"}
              [:user/id 2] {:user/id 2 :user/name "Second User"}}
             (#'sut/combine-rows
              [{:entity-attribute :user/id :entity-value 1 :attribute :user/name :value "User Name"}
               {:entity-attribute :user/id :entity-value 2 :attribute :user/name :value "Second User"}])))))


(def gen-named (gen/such-that #(some-> % name count (> 3)) (gen/one-of [gen/keyword gen/string gen/symbol]) 99))

(def gen-value (gen/one-of [gen/string-alphanumeric gen/small-integer]))
(def gen-row (gen/hash-map :entity-attribute gen-named
                           :entity-value gen-value
                           :attribute gen-named
                           :value gen-value))
(defspec combine-rows-spec
  (prop/for-all [{:keys [entity-attribute entity-value attribute value] :as row} gen-row]
                (= {[entity-attribute entity-value]
                    {entity-attribute entity-value attribute value}}
                   (#'sut/combine-rows [row]))))

(t/deftest upserted-rows-test
  (t/testing "Returns a row for every attribute/value pair"
    (t/is (= [{:entity-attribute :user/id :entity-value 1 :attribute :user/name :value "User Name"}]
             (#'sut/upserted-rows :user/id {:user/id 1 :user/name "User Name"})))
    (t/is (= #{{:entity-attribute :user/id :entity-value 1 :attribute :user/city :value "The City"}
               {:entity-attribute :user/id :entity-value 1 :attribute :user/name :value "User Name"}}
             (set (#'sut/upserted-rows :user/id {:user/id 1 :user/name "User Name" :user/city "The City"}))))
    (t/is (nil? (#'sut/upserted-rows :user/id {:user/id 1})))
    (t/is (thrown-with-msg? Throwable #"Assert failed" (#'sut/upserted-rows :user/id nil)))
    (t/is (thrown-with-msg? Throwable #"Assert failed" (#'sut/upserted-rows :user/id {:user/name "User Name"})))
    (t/testing "ignoring rows that are already present in db-rows"
      (t/is (= [{:entity-attribute :user/id :entity-value 1 :attribute :user/name :value "User Name"}]
               (let [rows [{:entity-attribute :user/id :entity-value 1 :attribute :user/city :value "The City"}]]
                 (#'sut/upserted-rows :user/id {:user/id 1 :user/name "User Name" :user/city "The City"} rows)))))))

(defn- row->map
  [{:keys [entity-attribute entity-value attribute value] :as _row}]
  {entity-attribute entity-value, attribute value})

(defspec upserted-rows-spec
  (prop/for-all
   [{:keys [entity-attribute] :as row} gen-row
    rows (gen/vector gen-row 1 99)]
   (= [row] (#'sut/upserted-rows entity-attribute (row->map row) rows))))
