(ns swark.authom-test
  (:require [clojure.test :as t]
            [swark.authom :as sut]))

(def USER #:user{:id 123 :fullname "User Name"})
(def USER-WITH-TOKEN (-> USER (sut/with-token :user/id "password" "SECRET")))
(def USER-TOKEN "-301775488")

(t/deftest token
  (t/testing "Returns the token if stored in metadata"
    (t/is (-> USER-WITH-TOKEN sut/token #{USER-TOKEN})))
  (t/testing "Returns nil if the token is NOT stored in metadata"
    (t/is (-> USER sut/token nil?))))

(t/deftest check-token
  (t/testing "Returns a truethy value when password and secret match"
    (t/is (-> USER-WITH-TOKEN (sut/check :user/id "password" "SECRET"))))
  (t/testing "Returns a falsey value when password and/or secret do not match"
    (t/is (-> USER-WITH-TOKEN (sut/check :user/id "wrong-password" "SECRET") not))
    (t/is (-> USER-WITH-TOKEN (sut/check :user/id "password" "WRONG_SECRET") not))))

(t/deftest enrich-pipeline
  (let [enriched (sut/disclose USER-WITH-TOKEN)
        restored (sut/conceal enriched)]
    (t/is (= restored USER-WITH-TOKEN))
    (t/is (= (meta USER-WITH-TOKEN) (meta restored)))))
