(ns swark.authom-test
  (:require [clojure.test :as t]
            [swark.authom :as sut]))

(def ITEM [:some :data])
(def WITH-TOKEN (-> ITEM (sut/with-token "password")))
(def TOKEN "1446530582")

(def USER #:user{:id 123 :fullname "User Name"})
(def USER-WITH-TOKEN (-> USER (sut/map-with-meta-token :user/id "password" "SECRET")))
(def USER-TOKEN "-301775488")

(t/deftest token
  (t/testing "Returns the token if stored in metadata"
    (t/is (-> WITH-TOKEN sut/token #{TOKEN}))
    (t/is (-> USER-WITH-TOKEN sut/token #{USER-TOKEN})))
  (t/testing "Returns nil if the token is NOT stored in metadata"
    (t/is (-> ITEM sut/token nil?))
    (t/is (-> USER sut/token nil?))))

(t/deftest check-token
  (t/testing "Returns a truethy value when password and secret match"
    (t/is (-> WITH-TOKEN (sut/check-token "password")))
    (t/is (-> USER-WITH-TOKEN (sut/map-check-meta-token :user/id "password" "SECRET"))))
  (t/testing "Returns a falsey value when password and/or secret do not match"
    (t/is (-> WITH-TOKEN (sut/check-token "wrong-password" "SECRET") not))
    (t/is (-> WITH-TOKEN (sut/check-token "password" "WRONG_SECRET") not))
    (t/is (-> USER-WITH-TOKEN (sut/map-check-meta-token :user/id "wrong-password" "SECRET") not))
    (t/is (-> USER-WITH-TOKEN (sut/map-check-meta-token :user/id "password" "WRONG_SECRET") not))))

(t/deftest enrich-pipeline
  (let [enriched (sut/enrich-token USER-WITH-TOKEN)
        restored (sut/restore-enriched-token enriched)]
    (t/is (= restored USER-WITH-TOKEN))
    (t/is (= (meta USER-WITH-TOKEN) (meta restored)))))
