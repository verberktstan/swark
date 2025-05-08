(ns swark.cedric2-test
  (:require [clojure.edn :as edn]
            [clojure.test :refer [deftest is testing]]
            [swark.cedric2 :as sut]))

(deftest row->entry
  (let [->entry (partial #'sut/row->entry ["id" "1" "initials" "A.B."])]
    (testing "row->entry returns the row as a map with {[ea ev] {a v}}"
      (is (= {["id" "1"] {"initials" "A.B."}} (->entry)))
      (is (= {[:id "1"] {:initials "A.B."}}
             (->entry :keywordize-attributes? true)))
      (is (= {[:id 1] {:initials "A.B."}}
             (->entry :keywordize-attributes? true
                      :ev-parser edn/read-string))))))

(deftest merge-rows
  (let [merge-rows* #(#'sut/merge-rows % [["id" "1" "initials" "A.B."]
                                          ["id" "1" "name" "Arthur Bent"]])]
    (testing "merge-rows returns a map of merged records keyed by their entity"
      (is (= {["id" "1"] {"initials" "A.B." "name" "Arthur Bent"}}
             (merge-rows* nil)))
      (is (= {[:id "1"] {:initials "A.B." :name "Arthur Bent"}}
             (merge-rows* {:keywordize-attributes? true})))
      (is (= {[:id 1] {:initials "A.B." :name "Arthur Bent"}}
             (merge-rows* {:keywordize-attributes? true
                           :ev-parser              edn/read-string}))))))
