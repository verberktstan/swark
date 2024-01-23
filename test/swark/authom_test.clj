(ns swark.authom-test
  (:require [clojure.test :as t]
            [swark.authom :as sut]))

(def ITEM [:some :data])
(def WITH-TOKEN (-> ITEM (sut/with-meta-token "password")))
(def TOKEN 1446530582)

(def USER #:user{:id 123 :fullname "User Name"})
(def USER-WITH-TOKEN (-> USER (sut/map-with-meta-token :user/id "password" "SECRET")))
(def USER-TOKEN -301775488)

(t/deftest meta-token
  (t/testing "Returns the token if stored in metadata"
    (t/is (-> WITH-TOKEN sut/meta-token #{TOKEN}))
    (t/is (-> USER-WITH-TOKEN sut/meta-token #{USER-TOKEN})))
  (t/testing "Returns nil if the token is NOT stored in metadata"
    (t/is (-> ITEM sut/meta-token nil?))
    (t/is (-> USER sut/meta-token nil?))))

(t/deftest check-meta-token
  (t/testing "Returns a truethy value when password and secret match"
    (t/is (-> WITH-TOKEN (sut/check-meta-token "password")))
    (t/is (-> USER-WITH-TOKEN (sut/map-check-meta-token :user/id "password" "SECRET"))))
  (t/testing "Returns a falsey value when password and/or secret do not match"
    (t/is (-> WITH-TOKEN (sut/check-meta-token "wrong-password" "SECRET") not))
    (t/is (-> WITH-TOKEN (sut/check-meta-token "password" "WRONG_SECRET") not))
    (t/is (-> USER-WITH-TOKEN (sut/map-check-meta-token :user/id "wrong-password" "SECRET") not))
    (t/is (-> USER-WITH-TOKEN (sut/map-check-meta-token :user/id "password" "WRONG_SECRET") not))))
