(ns swark.cedric-test
  (:require [clojure.edn :as edn]
            [clojure.test :refer [are deftest is testing]]
            [swark.cedric :as sut]
            [swark.core :as swark])
  (:import [swark.cedric Mem Csv]))

;; Parse :id record's category value as clojure object (int)
(remove-method sut/value-parser [:id :category])
(defmethod sut/value-parser [:id :category] [_]
  edn/read-string)

;; Parse user record's gender value as a keyword
(remove-method sut/value-parser [::user-id :user/gender])
(defmethod sut/value-parser [::user-id :user/gender] [_]
  keyword)

;; Parse primary-value as clojure object (int)
(remove-method sut/primary-value-parser ::user-id)
(remove-method sut/primary-value-parser :id)
(defmethod sut/primary-value-parser ::user-id [_]
  edn/read-string)
(defmethod sut/primary-value-parser :id [_]
  edn/read-string)

(deftest pipeline-test
  (let [user1 {:id 1 :username "Stan" :category 1}
        user2 {::user-id 2 :user/name "Nats" :user/gender :unknown}
        rows1 (#'sut/serialize {:primary-key :id} user1)
        rows2 (#'sut/serialize {:primary-key ::user-id} user2)]
    (are [result rows] (= result (->> rows
                                      (map (partial drop 1))
                                      (map (partial take 4))))
      [["id" "1" "username" "Stan"]
       ["id" "1" "category" "1"]]               rows1
      [["swark.cedric-test/user-id" "2" "user/name" "Nats"]
       ["swark.cedric-test/user-id" "2" "user/gender" "unknown"]] rows2)
    (is
     (= [{:username "Stan" :id 1 :category 1}
         {:user/name "Nats" ::user-id 2 :user/gender :unknown}]
        (#'sut/merge-rows (concat rows1 rows2))))))

(def ^:private NAMES #{"Alfa" "Bravo" "Charlie" "Delta" "Echo" "Foxtrot" "Golf" "Hotel" "India" "Juliett" "Kilo" "Lima" "Mike" "November" "Oscar" "Papa" "Quebec" "Romeo" "Sierra" "Tango" "Uniform" "Victor" "Whiskey" "X-ray" "Yankee" "Zulu"})

(defn- some-names
  ([]
   (some-names 2))
  ([n]
   (assert (< n 26))
   (->> NAMES shuffle (take n))))

(deftest implementation
  ;; Test all implementations in exactly the same way!
  (doseq [make-db [#(Mem. (atom nil))
                   #(Csv. (str "/tmp/testdb-" (swark/unid) ".csv"))]]
    (let [db        (make-db)
          db-conn   (swark/with-buffer db)
          transact! (partial swark/put! db-conn)
          props     {:primary-key :person/id}
          the-names (some-names 25)
          persons   (map (partial assoc nil :person/name) the-names)
          result    (transact! sut/upsert-items props persons)]
    ;; result
      (testing "upsert-items"
        (testing "returns the upserted items"
          (is (-> result count (= 25)))
          (is (->> result (map :person/name) set (= (set the-names))))))
      (let [new-names (some-names 3)
            persons (->> result
                         shuffle
                         (take 3)
                         (map #(assoc %2 :person/name %1) new-names))
            updated (transact! sut/upsert-items db props persons)]
        (testing "returns the updated items"
          (is (-> updated count #{3}))
          (is (->> updated (map :person/name) set (= (set new-names))))))
      (let [persons (->> result
                         shuffle
                         (take 5))
            archived (transact! sut/archive-items db props persons)]
        (testing "returns the number of ::archived items"
          (is (= {::sut/archived 5} archived))))
      (testing "returns all the items"
        (is (-> db (transact! sut/read-items {}) count #{20})))
      (swark/close! db-conn))))
