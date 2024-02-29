(ns swark.atomic-test
  (:require [clojure.test :as t]
            [swark.atomic :as sut]))

(t/deftest atomic
  (let [atomic    (sut/atomic "Hello" :in-buffer-size 2 :out-buffer-size 2)
        transact! (partial sut/put! atomic)]
    (t/is (= "Hello, World!" (transact! str ", " "World!")))
    (t/is (nil? (transact! :not-a-fn ", " "World!")))
    (t/is (= ::sut/closed! (sut/close! atomic))) ; Close it, after this every eval of put! returns nil
    (t/is (nil? (transact! str "silence")))))


