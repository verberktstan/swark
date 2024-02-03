(ns swark.cedric-test
  (:require [clojure.test :refer [deftest are is]]
            [swark.cedric :as sut]
            [clojure.edn :as edn]))

(remove-method sut/value-parser [:id :category])
(defmethod sut/value-parser [:id :category] [_]
  edn/read-string)

(remove-method sut/value-parser [:user/id :user/gender])
(defmethod sut/value-parser [:user/id :user/gender] [_]
  keyword)

(deftest pipeline-test
  (let [user1 {:id 1 :username "Stan" :category 1}
        user2 #:user{:id 2 :name "Nats" :gender :unknown}
        rows1 (sut/serialize {:primary-key :id} user1)
        rows2 (sut/serialize {:primary-key :user/id} user2)]
    (are [result rows] (= result (->> rows
                                      (map (partial drop 1))
                                      (map (partial take 4))))
      [["id" "1" "username" "Stan"]
       ["id" "1" "category" "1"]]               rows1
      [["user/id" "2" "user/name" "Nats"]
       ["user/id" "2" "user/gender" "unknown"]] rows2)
    (is
      (= {[:id 1]      {:username "Stan" :id 1 :category 1}
          [:user/id 2] #:user{:name "Nats" :id 2 :gender :unknown}} (sut/merge-rows (concat rows1 rows2))))))
