(ns swark.eav-test
  (:require [clojure.edn :as edn]
            [clojure.string :as str]
            [clojure.test :as t]
            [swark.core :as swark]
            [swark.eav :as sut]))

(def USER {:id 1 :username "Arnold"})
(def USER2 #:user{:id 2 :name "Arnold" :city "Birmingham"})

(t/deftest ->rows
  (t/is (= [[:id 1 :username "Arnold"]]
           (sut/->rows USER)))
  (t/is (= [[:user/id 2 :user/name "Arnold"]
            [:user/id 2 :user/city "Birmingham"]]
           (sut/->rows USER2 {:primary/key :user/id})))
  (t/is (= [[:city/id 3 :city/name "Birmingham"]
            [:city/id 4 :city/name "Cork"]]
           (mapcat
            #(sut/->rows % {:primary/key :city/id})
            [#:city{:id 3 :name "Birmingham"}
             #:city{:id 4 :name "Cork"}])))
  (t/is (= [["user/id" "2" "user/name" "Arnold"]
            ["user/id" "2" "user/city" "Birmingham"]]
           (sut/->rows USER2 {:primary/key :user/id
                              :entity/attribute swark/->str
                              :entity/value str
                              :attribute swark/->str
                              :value name}))))

(t/deftest parse-row
  (t/is (= [{:entity/attribute :id :entity/value 1 :attribute :username :value "Arnold"}]
           (map #'sut/parse-row (sut/->rows USER))))
  (t/is (= [{:entity/attribute :user/id :entity/value 2 :attribute :user/name :value "Arnold"}
            {:entity/attribute :user/id :entity/value 2 :attribute :user/city :value "Birmingham"}]
           (map #'sut/parse-row (sut/->rows USER2 {:primary/key :user/id}))))
  (t/is (= [{:entity/attribute :id :entity/value 1 :attribute "username" :value "Arnold"}]
           (map (partial #'sut/parse-row {:attribute name}) (sut/->rows USER))))
  (t/is (= [{:entity/attribute :user/id :entity/value :two :attribute "name" :value "Arnold"}
            {:entity/attribute :user/id :entity/value :two :attribute "city" :value "Birmingham"}]
           (map (partial #'sut/parse-row {:entity/value {2 :two} :attribute name}) (sut/->rows USER2 {:primary/key :user/id})))))

(t/deftest filter-eav
  (let [eav1 {:entity/attribute :id
              :entity/value 1
              :attribute :username
              :value "Arnold"}
        eav2 {:entity/attribute :user/id
              :entity/value 2
              :attribute :user/name
              :value "Bert"}]
    (t/are [result props item] (= result (#'sut/filter-eav props item))
      ;; Filtering based on (namespace part of) the :entity/attribute
      false {:entity/attribute (comp #{"user"} namespace)} eav1
      true  {:entity/attribute (comp #{"user"} namespace)} eav2

      ;; Filtering on the :entity/attribute and :entity/value
      true  {:entity/attribute #{:id} :entity/value #{1}} eav1
      false {:entity/attribute #{:id} :entity/value #{1}} eav2

      ;; Filtering on the :attribute
      false {:attribute (comp #{"user"} namespace)} eav1
      true  {:attribute (comp #{"user"} namespace)} eav2

      ;; Filtering on the :value
      true  {:value (comp #(str/starts-with? % "a") str/lower-case)} eav1
      false {:value (comp #(str/starts-with? % "a") str/lower-case)} eav2

      ;; Filtering on a combination of these
      false {:entity/attribute #{:user/id} :entity/value #{2} :attribute (comp #{"user"} namespace)} eav1
      true  {:entity/attribute #{:user/id} :entity/value #{2} :attribute (comp #{"user"} namespace)} eav2

      ;; Always match, when unsupported props are supplied
      true {:something "else"} eav1)))

(t/deftest merge-rows
  (t/testing "Parse, filter and merge rows from a line-seq (e.g. csv-rows)"
    (t/is (= {[:id 1] {:id 1 :username "Arnold"}
              [:user/id 2] #:user{:id 2 :name "Bert"}}
             (sut/merge-rows
              {:entity/attribute keyword :entity/value edn/read-string :attribute keyword :value identity}
              nil
              [["id" "1" "username" "Arnold"]
               ["user/id" "2" "user/name" "Bert"]])))

    (t/is (= {[:id 1] {:id 1 :username "Arnold"}}
             (sut/merge-rows
              {:entity/attribute keyword :entity/value edn/read-string :attribute keyword :value identity}
              {:entity/value #{1}}
              [["id" "1" "username" "Arnold"]
               ["user/id" "2" "user/name" "Bert"]])))

    (t/is (= {[:user/id 2] #:user{:id 2 :name "Bert"}}
             (sut/merge-rows
              {:entity/attribute keyword :entity/value edn/read-string :attribute keyword :value identity}
              {:entity/attribute (comp #{"user"} namespace)}
              [["id" "1" "username" "Arnold"]
               ["user/id" "2" "user/name" "Bert"]])))))
