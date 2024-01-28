(ns swark.eav-test
  (:require [clojure.test :as t]
            [swark.eav :as sut]))

(def USER {:id 1 :username "Arnold"})
(def USER2 #:user{:id 2 :name "Arnold" :city "Birmingham"})

(t/deftest ->rows
  (t/is (= [[:id 1 :username "Arnold"]]
           (sut/->rows :id USER)))
  (t/is (= [[:user/id 2 :user/name "Arnold"]
            [:user/id 2 :user/city "Birmingham"]]
           (sut/->rows :user/id USER2)))
  (t/is (= [[:city/id 3 :city/name "Birmingham"]
            [:city/id 4 :city/name "Cork"]]
           (mapcat
             (partial sut/->rows :city/id)
             [#:city{:id 3 :name "Birmingham"}
              #:city{:id 4 :name "Cork"}]))))

(t/deftest parse-row
  (t/is (= [{:entity/attribute :id :entity/value 1 :attribute :username :value "Arnold"}]
           (map sut/parse-row (sut/->rows :id USER))))
  (t/is (= [{:entity/attribute :user/id :entity/value 2 :attribute :user/name :value "Arnold"}
            {:entity/attribute :user/id :entity/value 2 :attribute :user/city :value "Birmingham"}]
           (map sut/parse-row (sut/->rows :user/id USER2))))
  (t/is (= [{:entity/attribute :id :entity/value 1 :attribute "username" :value "Arnold"}]
           (map (partial sut/parse-row {:attribute name}) (sut/->rows :id USER))))
  (t/is (= [{:entity/attribute :user/id :entity/value :two :attribute "name" :value "Arnold"}
            {:entity/attribute :user/id :entity/value :two :attribute "city" :value "Birmingham"}]
           (map (partial sut/parse-row {:entity/value {2 :two} :attribute name}) (sut/->rows :user/id USER2)))))
