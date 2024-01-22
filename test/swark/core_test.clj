(ns swark.core-test
  (:require [clojure.test :as t]
            [swark.core :as sut]))

(t/deftest key-by
  (let [records        [{:id 0 :name "ab"} {:id 1 :name "cd"}]
        number-vectors [[1 1] [1 2] [2 3 5] [3 5 8 13]]]
    (t/testing "Returns a map with items keyed-by f"
      (t/is
       (= {0 {:id 0, :name "ab"}, 1 {:id 1, :name "cd"}}
          (sut/key-by :id records)))

      (t/is
       (= {2 [1 1], 3 [1 2], 10 [2 3 5], 29 [3 5 8 13]}
          (sut/key-by (partial reduce +) number-vectors)))

      (t/testing "..doesn't include items keyed by `nil`"
        (t/is
         (= {12 #:user{:id 12, :name "u12"}, 23 #:user{:id 23, :name "u23"}}
            (sut/key-by
             :user/id
             [#:user{:id 12 :name "u12"}
              #:user{:id 23 :name "u23"}
              {:id 34 :name "not-included!"}])))))

    (t/is (thrown? AssertionError (sut/key-by nil records)))
    (t/is (nil? (sut/key-by :id nil)))))

(t/deftest map-vals
  (let [m {:a [1 1] :b [1 2] :c [2 3 5] :d [3 5 8 13]} #_{:a 1 :b 2 :c 3}]
    (t/testing "Returns the map with f applied to all it's vals"
      (t/is (= {:a 2, :b 2, :c 3, :d 4} (sut/map-vals count m)))
      (t/is (= {:a 2, :b 3, :c 10, :d 29}
               (sut/map-vals
                (partial reduce +) m))))

    (t/is (thrown? AssertionError (sut/map-vals nil m)))
    (t/is (nil? (sut/map-vals inc nil)))))

(t/deftest ->str
  (t/are [result input] (= result (sut/->str input))
    "Hello, Swark!" "Hello, Swark!"
    "keyword2"      :keyword2
    "symbol3"       'symbol3
    "string4"       " string4  "
    nil             "  "
    nil             nil))

(t/deftest ->keyword
  (t/are [result match replacement input] (= result (sut/->keyword match replacement input))
    :test     nil          nil :test
    :hello    nil          nil "hello"
    :symbol   nil          nil 'symbol
    :h-ell-o1 nil          nil " H ell-o1"
    :hello    #"[0-9\s\-]" ""  " H ell-o1"))

(t/deftest spec
  (t/are [result spec input] (= result (sut/invalid? spec input))
    nil       {:id nat-int?} {:id 0} ; Valid, so `nil` is returned
    #{:id}    {:id nat-int?} {:id -1} ; Invalid, so a set of invalid keys is returns
    ::sut/nil {}             nil ; Nil input, so ::swark/nil is returned
    ::sut/nil {:id nat-int?} nil)
  (t/are [msg spec input] (thrown-with-msg? AssertionError msg (sut/valid? spec input))
    #"Spec should be a map!" nil {:id -1}
    #"All vals in spec should implement IFn" {:id "not IFn"} {:id -1} ; Spec
    #"Item should be a map!" {:id nat-int?} false))
