(ns swark.eav
  {:added "0.1.3"
   :doc   "Serialize and parse data as entity-atteibute-value rows."}
  (:require [clojure.data :as data]
            [swark.core :refer [->str unid]]
            [clojure.data.csv :as csv]
            [clojure.java.io :as io]
            [clojure.edn :as edn]
            #?(:clj [clojure.string :as str])
            #?(:clj [clojure.set :as set]))
  #?(:clj (:import [java.time LocalDateTime format.DateTimeFormatter])))

(comment
  (def USER #:user{:id 123 :name "Stan" :age 36})
  )

(defn- parse [parser & args] (apply parser args))

;; Step 1. Serialize the record, unparse first, then convert as row(s)
(defn- unparse
  [{val-parsers :val/parsers
    parse-key   :parse/key
    primary-key :primary/key
    :or         {parse-key ->str}
    :as         props}
   item]
  (let [parsers (select-keys val-parsers (keys item))]
    (->> item
         (merge-with parse parsers)
         (map (juxt (comp parse-key key) val))
         (into {}))))

(defn- now-str []
  #?(:cljs (.toISOString (js/Date.))
     :clj (.format (LocalDateTime/now) DateTimeFormatter/ISO_LOCAL_DATE_TIME)))

(defn- unparse-flags
  [flags]
  (when (seq flags)
    (mapv (comp symbol ->str) flags)))

(defn serialize
  "Returns serialized rows for the map item."
  [{parse-key   :unparse/key
    primary-key :primary/key
    tx          :transaction/id
    flags       :row/flags
    parse-flags :unparse/flags
    :or         {tx          (now-str)
                 parse-key   ->str
                 parse-flags (comp ->str unparse-flags)}
    :as         props}
   item]
  (let [unparsed (unparse props item)
        pk       (parse-key primary-key)
        entity   (find unparsed pk)]
    (assert entity)
    (map
      #(vec (concat [tx] entity % [(parse-flags flags)]))
      (dissoc unparsed pk))))

(comment
  (def ROWS
    (concat
      (serialize {:primary/key :user/id
                  :parse/key   ->str
                  :val/parsers {:user/id  str
                                :user/age str}
                  :row/flags   #{#_::deleted}} USER)
      (serialize {:primary/key :user/id
                  :parse/key   ->str
                  :val/parsers {:user/id  str
                                :user/age str}
                  :row/flags   #{::deleted}} (select-keys USER [:user/id :user/age]))
      (serialize {:primary/key :customer/id} {:customer/id "abc123" :customer/name "Stan"})))
  )

;; Step 2. Eat your own dogfood. Turn serialized record into a map.
(defn- parse-flags* [s]
  (when-not (str/blank? s)
    (->> s
         edn/read-string
         (map keyword)
         set)))

(comment
  (parse-flags* "[test/ikel]")
)

(defn row-parser
  [{attribute-parsers :attribute/parsers
    value-parsers     :value/parsers
    parse-ea          :parse/entity-attribute
    parse-ev          :parse/entity-value
    parse-flags       :parse/flags
    :or               {parse-ea keyword parse-ev identity parse-flags parse-flags*}
    :as               props}
   [tx entity-attribute entity-value attribute value flags]]
  (let [ea     (parse-ea entity-attribute)
        entity [ea (parse-ev entity-value)]
        flags' (parse-flags flags)]
    (assert entity)
    (let [parse-attribute (get attribute-parsers ea keyword)
          attribute'      (parse-attribute attribute)
          parse-value     (get value-parsers [ea attribute'] identity)]
      {entity (->> flags'
                   (map #(vector % attribute'))
                   (into [entity])
                   (into {attribute' (parse-value value)}))})))

(comment
  (map
    (partial row-parser {:attribute/parsers  {:user/id keyword}
                         :value/parsers      {[:user/id :user/age] edn/read-string}
                         :parse/entity-value edn/read-string}) ROWS)
  )

#_(defn- mapify
    [[primary-key _] item]
    (let [entity (find item primary-key)]
      (assert entity)
      {entity item}))

(comment
  (mapify [:user/id 123] {:user/id 123 :user/name "Stan" ::deleted :user/name})
  )

(defn- assert-ifn-vals
  [props]
  (when (seq props)
    (doseq [[k f] props]
      (-> f ifn? (assert (str k " does not implement IFn!"))))))

(defn- only-ifn-vals [props]
  (->> props (filter (comp ifn? val)) (into {})))

(comment
  (->> (merge-with parse {:some/thing str} (select-keys {:test "ikel"} [:some/thing 'else]))
       (juxt :some/thing))
  )

(defn- filter-eav
  [props m]
  (if-not (seq props)
    true
    (let [props' (only-ifn-vals props)
          item   (some-> m vals first)
          keyseq (keys props')
          keys'  (set/intersection (-> item keys set) (-> props' keys set))
          values (if (seq keys')
                   (apply juxt keys')
                   (fn [_] (vector nil)))]
      (some->> (select-keys item keyseq)
               (merge-with parse props')
               values
               seq
               (every? identity)))))

(comment
  (filter-eav {#_#_#_#_:user/id #{123} :user/name #{"Stan"} :company/id #{123}} {[:user/id 123] #:user{:id 123 :name "Stan"}})
  )

(defn filterer
  [props]
  (filter (partial filter-eav props)))

(comment
  (filter-eav {:user/id #{123}} {[:user/id 123] {:user/id 123}})
  (filter-eav {:user/id #{123}} {[:user/id 123] {:customer/id 234}})
  (filter (partial filter-eav {:company/name #{"ABC"}}) [{[:user/id 123] {:user/id 123}}
                                                         {[:company/name "ABC"] {:company/name "ABC"}}])
  )

(defn parser
  [props]
  (map (partial row-parser props)))

(defn- merge-rows* [map1 {::keys [deleted] :as map2}]
  (cond
    (and deleted (contains? map1 deleted))
    (dissoc map1 deleted)
    deleted
    map1
    :else
    (merge map1 map2)))

(defn merge-rows
  "Returns eagerly parsed and merged rows."
  ([rows]
   (merge-rows nil rows))
  ([parse-props rows]
   (merge-rows parse-props nil rows))
  ([parse-props filter-props rows]
   (transduce
     (comp
       (parser parse-props)
       (filterer filter-props))
     (partial merge-with merge-rows*)
     rows)))

(comment
  (merge-rows {:parse/entity-value edn/read-string
               :value/parsers      {[:user/id :user/age] edn/read-string}} #_{:user/id #{123}} ROWS)

  (merge-rows {:parse/entity-value edn/read-string
               :value/parsers      {[:user/id :user/age] edn/read-string}} {:user/id any?} ROWS)
  )

(defn upsert [{pk     :primary/key
               ea-gen :entity-attribute/generator
               :or    {ea-gen unid} :as props} item rows]
  (assert pk)
  (let [db      (merge-rows props {pk any?} rows)
        update? (contains? item pk)
        entity  (if update? (find item pk) [pk (ea-gen)])]
    entity))

(comment
  (upsert {:primary/key        :user/id
           :parse/entity-value edn/read-string
           :value/parsers      {[:user/id :user/age] edn/read-string}} {:test "ikel"} ROWS)
  )

#_(defn rows
    [{pk    :primary/key
      tx    :transaction/id
      flags :rows/flags
      :or   {tx #?(:cljs (.toISOString (js/Date.))
                   :clj (.format (LocalDateTime/now) DateTimeFormatter/ISO_LOCAL_DATE_TIME))}
      :as   props} map]
    ;; (-> map map? assert)
    (let [entity (find map pk)]
      (assert entity)
      (clojure.core/map
        #(vec (concat [tx] entity % [flags]))
        (dissoc map pk))))

#_(defn serialize
    [{ea-parser   :entity/attribute-parser
      ev-parsers  :entity/value-parsers
      att-parsers :attribute/parsers
      :or         {ea-parser   ->str
                   ev-parsers  {:user/id edn/read-string}
                   att-parsers {:user/id {"user/age" edn/read-string}}}
      :as         props} [tx ea ev att val flags :as row]]
    (let [entity-attribute (ea-parser ea)
          ev-parser        (get ev-parsers entity-attribute identity)]
      [tx
       entity-attribute
       (ev-parser ev)
       (att-parser att)]))

#_(defn parse
    [props [tx ea ev att val flags :as row]]
    [])

(comment
  (pr-str (mapv ->str #{:test}))
  (mapv ->str nil)
  (clojure.edn/read-string (pr-str (->str :id)))
  (let [rowz (rows {:primary/key :id} {:id 0 :name "Stan"})]
    rowz)

  (let [rowz (rows {:primary/key :user/id :rows/flags #{::removed}} {:user/id 0 :user/name "Stan" :user/age 36})]
    (with-open [writer (io/writer "testout.csv")]
      (csv/write-csv writer (map (partial serialize nil) rowz))))

  (with-open [reader (io/reader "testout.csv")]
    (doall
      (csv/read-csv reader)))

  ;; TODO: Serialize (to csv?)
  (with-open [writer (io/writer "testout.csv")]
    (csv/write-csv writer [["a" "b"]]))
  )

#_(comment
    (with-open [reader (io/reader "testdata.csv")]
      (merge-rows (csv/read-csv reader)))

    (with-open [reader (io/reader "testdata.csv")]
      (merge-rows
        {:entity/attribute keyword
         :entity/value     clojure.edn/read-string
         :attribute        keyword
         }
        (csv/read-csv reader))))

;; Storing data as Entity / Attribute / Value rows
;; The goal is to have an append only (atomic) database

;; TODO: Make it work with CSV, give an example

#_(defn- parse [f input] (cond-> input (ifn? f) f))

;; TODO: Add test for append arguments
#_(defn ->rows
    "Returns a sequence of vectors with [entity-attribute entity-value attribute value]
  for each map-entry in map m."
    ([m] (->rows m nil))
    ([m {primary-key            :primary/key
         parse-entity-attribute :entity/attribute
         parse-entity-value     :entity/value
         parse-attribute        :attribute
         parse-value            :value
         :or                    {primary-key            :id
                                 parse-entity-attribute identity parse-entity-value identity
                                 parse-attribute        identity parse-value        identity}}]
     (let [entry (find m primary-key)]
       (assert entry "Mapentry can't be found!")
       (let [entry (mapv parse [parse-entity-attribute parse-entity-value] entry)]
         (->> (dissoc m primary-key)
              (map (partial mapv parse [parse-attribute parse-value]))
              (map (partial into entry))))))
    ([m props & append]
     (->> (->rows m props)
          (map #(apply conj % append)))))

(comment
  (->rows {:test "ikel" :some "thing"} {:primary/key :test} ::deleted)
  )

;; TODO: Make parse-row and merge-rows swallow this ::removed flag
#_(defn- diff [primary-key & maps]
    (let [entity            (-> (apply merge maps) (find primary-key))
          [removed added _] (apply data/diff maps)
          removed           (some->> removed
                                     (remove (comp (or added {}) key))
                                     seq
                                     (into {}))]
      {::removed removed
       ::added   added}))

#_(defn diff-rows
    [{primary-key :primary/key :as props} & maps]
    (let [diff*   (apply diff primary-key maps)
          entity  (select-keys (apply merge maps) [primary-key])
          removed (-> diff* ::removed (merge entity))
          added   (-> diff* ::added (merge entity))]
      (concat
        (->rows removed props (->str ::removed))
        (->rows added props))))

(comment
  (diff-rows {:primary/key :id} {:id 0 :ab "cd" :cd "ef"} {:id 0 :ab "de"}) 
  )

#_(defn- assert-ifn-vals
  [props]
  (when (seq props)
    (doseq [[k f] props]
      (-> f ifn? (assert (str k " does not implement IFn!"))))))

#_(defn- parse-row
  "Returns row as a map with :entity/attribute, :entity/value, :attribute & :value. Applies supplied parsers on the fly for thise mapentries. You can supply a value parser lookup via :value/parsers, if a parser can be found by [:entity/attribute :attribute], this is used to parse the :value of the row's eav-map."
  ([row]
   (parse-row nil row))
  ([{:value/keys [parsers] :as props} row]
   (let [keyseq [:entity/attribute :entity/value :attribute :value ::option]
         props     (select-keys props keyseq)
         _         (assert-ifn-vals props)
         item      (->> row
                     (zipmap keyseq)
                     (merge-with parse (assoc props ::option keyword)))
         ea-vector (juxt :entity/attribute :attribute)
         v-parser  (get parsers (ea-vector item))]
     (cond-> (dissoc item ::option)
       v-parser (update :value v-parser)
       (some-> item ::option #{::removed}) (assoc ::removed true)))))

#_(defn parser
  [props]
  (map (partial parse-row props)))

#_(defn- filter-eav
  [props eav-map]
  (let [props  (select-keys props (keys eav-map))
        keyseq (keys props)
        filter-vals (when (seq keyseq) (apply juxt keyseq))]
    (assert-ifn-vals props)
    (if-not (seq props)
      true ; Always match when there are no valid props to filter on.
      (->> (select-keys eav-map keyseq)
           (merge-with parse props)
           filter-vals
           (every? identity)))))

#_(defn filterer
  [props]
  (filter (partial filter-eav props)))

#_(defn- mapify
  [{ea  :entity/attribute
    ev  :entity/value
    a   :attribute
    v   :value
    rm? ::removed}]
  {[ea ev] (cond-> {a v}
             rm?       (assoc ::removed true)
             (not rm?) (assoc ea ev))})

#_(defn- merge-rows* [map1 {::keys [removed] :as map2}]
  (if removed
    (apply dissoc map1 (keys map2))
    (merge map1 map2)))

#_(defn merge-rows
  ([rows]
   (merge-rows nil rows))
  ([parse-props rows]
   (merge-rows parse-props nil rows))
  ([parse-props filter-props rows]
   (transduce
     (comp
       (parser parse-props)
       (filterer filter-props)
       (map mapify))
     (partial merge-with merge-rows*)
     rows)))
