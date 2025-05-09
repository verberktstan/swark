(ns swark.cedric2-test
  (:require [clojure.edn :as edn]
            [clojure.test :refer [deftest is testing]]
            [swark.cedric2 :as sut]))

(def ^:private rows1 [["id" "1" "initials" "A.B."]
                     ["id" "1" "name" "Arthur Bent"]])

(def ^:private record1
  (#'sut/merge-rows
   {:ev-parser              edn/read-string
    :keywordize-attributes? true}
   rows1))

(def ^:private records1 (#'sut/->set record1))

(deftest row->entry
  (let [->entry (partial #'sut/row->entry (first rows1))]
    (testing "row->entry returns the row as a map with {[ea ev] {a v}}"
      (is (= {["id" "1"] {"initials" "A.B."}} (->entry)))
      (is (= {[:id "1"] {:initials "A.B."}}
             (->entry :keywordize-attributes? true)))
      (is (= {[:id 1] {:initials "A.B."}}
             (->entry :keywordize-attributes? true
                      :ev-parser edn/read-string))))))

(deftest merge-rows
  (let [merge-rows* #(#'sut/merge-rows % rows1)]
    (testing "returns a map of merged records keyed by their entity"
      (is (= {["id" "1"] {"initials" "A.B." "name" "Arthur Bent"}}
             (merge-rows* nil)))
      (is (= {[:id "1"] {:initials "A.B." :name "Arthur Bent"}}
             (merge-rows* {:keywordize-attributes? true})))
      (is (= {[:id 1] {:initials "A.B." :name "Arthur Bent"}} record1)))
    (testing "returns only records with matching entity when requested"
      (is (= {[:id 1] {:initials "A.B." :name "Arthur Bent"}}
             (#'sut/merge-rows
              {:ev-parser              edn/read-string
               :keywordize-attributes? true
               :entities               records1}
              rows1)))
      (is (nil? (#'sut/merge-rows
                  {:ev-parser              edn/read-string
                   :keywordize-attributes? true
                   :entities               #{{:id 2}}}
                  rows1))))))

(deftest ->set
  (testing "->set returns a set with record merged with their entity"
    (is (= #{{:id 1 :initials "A.B." :name "Arthur Bent"}}
           records1))))
