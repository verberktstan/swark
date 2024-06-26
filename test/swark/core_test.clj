(ns swark.core-test
  (:require [clojure.edn :as edn]
            [clojure.test :as t]
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

(t/deftest filter-keys
  (let [map {:user-id 1 :user/name "Username" ::test "Testdata"}
        ns-str (namespace ::this)]
    (t/are [result f] (= result (sut/filter-keys map f))
      {:user-id 1}            (complement namespace)
      {:user/name "Username"} (comp #{"user"} namespace)
      {::test "Testdata"}     (comp #{ns-str} namespace)
      {}                     (comp #{"unknown"} namespace))))

(t/deftest select-namespaced
  (let [map {:user-id 1 :user/name "Username" ::test "Testdata"}]
    (t/are [result ns] (= result (sut/select-namespaced map ns))
      {:user-id 1}            nil
      {:user/name "Username"} "user"
      {::test "Testdata"}     (namespace ::this)
      {} "unknown")))

(t/deftest with-retries
  (t/testing "Returns a map with the :result, :n input and :retries-left"
    (t/is (= {:result       (inc 1)
              :n            3
              :retries-left 3}
             (sut/with-retries 3 inc 1))))
  (let [{:keys [throwable n retries-left]} (sut/with-retries 4 / 1 0)]
    (t/testing "Returns the n input and n retries left"
      (t/is (= 4 n))
      (t/is (= retries-left 0)))
    (t/testing "Returns a throwable in case of an error or exception"
      (t/is (= #{:via :trace :cause} (-> throwable keys set)))
      (t/is (-> throwable :cause #{"Divide by zero"})))))

(t/deftest ->str
  (t/are [result input] (= result (sut/->str input))
    "Hello, Swark!" "Hello, Swark!"
    "keyword2"           :keyword2
    "user/id"            :user/id
    "swark.core-test/id" ::id
    "symbol3"            'symbol3
    "string4"            " string4  "
    nil                  "  "
    nil                  nil))

(t/deftest unid
  (t/is (string? (sut/unid)))
  (t/is (-> #{"x"} sut/unid count #{1}))
  (t/is (-> {"x" :val-of-x} sut/unid count #{1})) ;; Works with maps as well
  (t/is (->> #{"xyzab"} (sut/unid {:min-length 5}) count (>= 5)))
  (t/is (->> {"xyzab" :val-of-xyzab} (sut/unid {:min-length 5}) count (>= 5)))
  (t/is (-> (reduce (fn [x _] (conj x (sut/unid x))) #{} (range 999)) count #{999}))
  (t/is (-> (reduce (fn [x _] (assoc x (sut/unid x) :another-val)) {} (range 999)) count #{999}))
  (t/is (-> (reduce (fn [x _] (conj x (sut/unid {:min-length 4} x))) #{} (range 999)) count #{999}))
  (t/is (-> (reduce (fn [x _] (assoc x (sut/unid {:min-length 4} x) :another-val)) {} (range 999)) count #{999}))
  (let [three-digits (sut/unid {:min-length 3 :no-dashes? true :filter-regex #"\d"} #{})
        four-letters (sut/unid {:min-length 4 :no-dashes? true :filter-regex #"\D"} #{})
        five-chars   (sut/unid {:min-length 5} #{})]
    (t/is (-> three-digits count #{3}))
    (t/is (->> three-digits seq (every? (comp number? edn/read-string str))))
    (t/is (-> four-letters count #{4}))
    (t/is (->> four-letters seq (every? (comp (partial re-find #"\D") str))))
    (t/is (-> five-chars count #{5}))
    (t/is (->> five-chars seq (every? (comp (partial re-find #"[a-z]|[0-9]|\-") str))))))

(t/deftest ->keyword
  (t/are [result args] (= result (apply sut/->keyword args))
    :test         [:test]
    ::test        [::test]
    :hello        ["hello"]
    :symbol       ['symbol]
    :h-ell-o1     [" H ell-o1"]
    :test/h-ell-o ["test/h ell o"]
    :he--o        [#"!" "he!!o"]
    :hello        [#"!" "l" "he!!o"]
    :test/hello   [#"!" "l" "test/he!!o"]
    :hello        [#"[0-9\s\-]" ""  " H ell-o1"]))

(t/deftest spec
  (let [report #::sut{:predicate nat-int? :input -1 :result false}]
  (t/are [result spec input] (= result (sut/invalid-map? spec input))
    nil       {:id nat-int?} {:id 0} ; Valid, so `nil` is returned
    {:id report} {:id nat-int?} {:id -1} ; Invalid, so a set of invalid keys is returns
    ::sut/nil    {}             nil ; Nil input, so ::swark/nil is returned
    ::sut/nil {:id nat-int?} nil)
  (t/are [msg spec input] (thrown-with-msg? AssertionError msg (sut/valid-map? spec input))
    #"Spec should be a map!" nil {:id -1}
    #"All vals in spec should implement IFn" {:id "not IFn"} {:id -1} ; Spec
    #"Input should be a map!" {:id nat-int?} false)))

(t/deftest memoir
  (let [random (sut/memoir rand-int)
        x      (random 999) ; The result is cached
        y      (random 9999)]
    (t/testing "Returns the cached input"
      (t/is (= x (random 999)))
      (t/is (= y (random 9999))))
    (t/testing "Flush a specific subset of the cache"
      (t/is (= x (random :flush 999))) ; Returns the flushed subset of the cache
      (t/is (not= x (random 999))) ; Caches a new result
      (t/is (= (random 999) (random 999))) ; Returns the new cached input
      (t/is (nil? (random :flush 99)))) ; Returns nil if the cache to flush subset is nonexistent
    (t/testing "Flush the complete cache"
      (t/is (nil? (random :flush)))
      (t/is (not= y (random 9999))))))
