(ns swark.authom-test
  (:require [clojure.test :as t]
            [swark.authom :as sut]))

(def ITEM #:user{:id 123 :fullname "User Name"})
(def WITH-TOKEN (-> ITEM (select-keys [:user/id]) (sut/with-meta-token "password" "SECRET")))
(def TOKEN -301775488)

(t/deftest meta-token
  (t/testing "Returns the token if stored in metadata"
    (t/is (= TOKEN (sut/meta-token WITH-TOKEN))))
  (t/testing "Returns nil if the token is NOT stored in metadata"
    (t/is (nil? (sut/meta-token ITEM)))))

(t/deftest check-meta-token
  (t/testing "Returns a truethy value when password and secret match"
    (t/is (sut/check-meta-token WITH-TOKEN "password" "SECRET")))
  (t/testing "Returns a falsey value when password and/or secret do not match"
    (t/is (not (sut/check-meta-token WITH-TOKEN "wrong-password" "SECRET")))
    (t/is (not (sut/check-meta-token WITH-TOKEN "password" "WRONG_SECRET")))))
