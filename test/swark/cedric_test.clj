(ns swark.cedric-test
  (:require [clojure.edn :as edn]
            [clojure.test :refer [deftest are is testing]]
            [swark.cedric :as sut]))

;; Parse :id record's category value as clojure object (int)
(remove-method sut/value-parser [:id :category])
(defmethod sut/value-parser [:id :category] [_]
  edn/read-string)

;; Parse user record's gender value as a keyword
(remove-method sut/value-parser [:user/id :user/gender])
(defmethod sut/value-parser [:user/id :user/gender] [_]
  keyword)

;; Parse primary-value as clojure object (int)
(remove-method sut/primary-value-parser :user/id)
(remove-method sut/primary-value-parser :id)
(defmethod sut/primary-value-parser :user/id [_]
  edn/read-string)
(defmethod sut/primary-value-parser :id [_]
  edn/read-string)

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
      (= [{:username "Stan" :id 1 :category 1}
          #:user{:name "Nats" :id 2 :gender :unknown}]
         (sut/merge-rows (concat rows1 rows2))))))
