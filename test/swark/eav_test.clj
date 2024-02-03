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
           (sut/->rows USER2 {:primary/key      :user/id
                              :entity/attribute swark/->str
                              :entity/value     str
                              :attribute        swark/->str
                              :value            name})))
  (t/is (= [["id" "1" "username" "Arnold" "swark.eav/removed"]]
           (sut/->rows USER {:primary/key      :id
                             :entity/attribute swark/->str
                             :entity/value     str
                             :attribute        swark/->str
                             :value            name}
                       (swark/->str ::sut/removed)))))

(t/deftest diff
  (t/is (= {::sut/removed {:user/city "Birmingham"}
            ::sut/added   {:user/name "Bert"}}
           (#'sut/diff :user/id USER2 #:user{:id 2 :name "Bert"}))))

(t/deftest diff-rows
  (t/is (= [[:user/id 2 :user/city "Birmingham" "swark.eav/removed"]]
           (sut/diff-rows {:primary/key :user/id} USER2 #:user{:id 2 :name "Arnold"})))
  (t/is (= [[:user/id 2 :user/city "Birmingham" "swark.eav/removed"]
            [:user/id 2 :user/name "Bert"]]
           (sut/diff-rows {:primary/key :user/id} USER2 #:user{:id 2 :name "Bert"}))))

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
           (map (partial #'sut/parse-row {:entity/value {2 :two} :attribute name}) (sut/->rows USER2 {:primary/key :user/id}))))
  (t/is (= {:entity/attribute :user/id
            :entity/value     2
            :attribute        :user/type
            :value            :member}
           (#'sut/parse-row
             {:entity/attribute swark/->keyword
              :entity/value     edn/read-string
              :attribute        swark/->keyword
              :value/parsers    {[:user/id :user/type] swark/->keyword}}
             ["user/id" "2" "user/type" "member"])))
  (t/is (= {:entity/attribute :user/id
            :entity/value     2
            :attribute        :user/type
            :value            :member
            ::sut/removed     true}
           (#'sut/parse-row
             {:entity/attribute swark/->keyword
              :entity/value     edn/read-string
              :attribute        swark/->keyword
              :value/parsers    {[:user/id :user/type] swark/->keyword}}
             ["user/id" "2" "user/type" "member" "swark.eav/removed"]))))

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
  (let [users1-rows [["name" "Arnold" "city" "Birmingham"]
                     ["name" "Christoff" "city" "Durness"]
                     ["name" "Arnold" "age" "31"]
                     ["name" "Arnold" "age" "31" "swark.eav/removed"]]
        users2-rows [[:test/id 1 :test/abc "def"]
                     [:test/id 2 :test/ghi "jkl"]]]
    (t/testing "Simply merges the rows, without parsing, without filtering."
      (t/is (= {["name" "Arnold"]    {"name" "Arnold" "city" "Birmingham"}
                ["name" "Christoff"] {"name" "Christoff" "city" "Durness"}}
               (sut/merge-rows users1-rows)))
      (t/is (= {[:test/id 1] #:test{:id 1 :abc "def"}
                [:test/id 2] #:test{:id 2 :ghi "jkl"}}
               (sut/merge-rows users2-rows)))))
  (t/testing "Parse, filter and merge rows from a line-seq (e.g. csv-rows)"
    (t/is (= {[:id 1]      {:id 1 :username "Arnold"}
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
                ["user/id" "2" "user/name" "Bert"]])))

    (t/is (= {[:user/id 2] #:user{:id 2 :name "Bert"}
              [:id 1]      {:id 1 :city "Birmingham"}}
             (sut/merge-rows
               {:entity/attribute keyword :entity/value edn/read-string :attribute keyword :value identity}
               [["id" "1" "username" "Arnold"]
                ["id" "1" "city" "Birmingham"]
                ["user/id" "2" "user/name" "Bert"]
                ["id" "1" "username" "Arnold" "swark.eav/removed"]])))))
