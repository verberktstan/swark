(ns swark.core-test
  (:require [clojure.test :as t]
            [swark.core :as sut]))

(t/deftest key-by
  (let [records        [{:id 0 :name "ab"} {:id 1 :name "cd"}]
        number-vectors [[1 1] [1 2] [2 3 5] [3 5 8 13]]]
    (t/is
      (= {0 {:id 0, :name "ab"}, 1 {:id 1, :name "cd"}}
         (sut/key-by :id records)))

    (t/is (thrown? AssertionError (sut/key-by nil records)))

    (t/is (nil? (sut/key-by :id nil)))

    (t/is
      (= {2 [1 1], 3 [1 2], 10 [2 3 5], 29 [3 5 8 13]}
         (sut/key-by (partial reduce +) number-vectors)))
    (t/is
      (= {12 #:user{:id 12, :name "u12"}, 23 #:user{:id 23, :name "u23"}}
         (sut/key-by
           :user/id
           [#:user{:id 12 :name "u12"}
            #:user{:id 23 :name "u23"}
            {:id 34 :name "not-included!"}])))))
