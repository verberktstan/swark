(ns swark.cedric
  (:require [swark.core :as swark]
            [clojure.edn :as edn]
            [clojure.set :as set]
            [clojure.data :as data]
            #?(:cljs [goog.date :as gd])
            #?(:clj [clojure.string :as str]))
  #?(:clj (:import [java.time Instant])))

;; TODO: Test in cljs as well
;; TODO: Move back in time by filtering on txd (transaction's utc date)
;; TODO: Implement destroy with ::archive flag
;; TODO: Write rows to csv, and test with that.
;; TODO: Make Read, Upsert & Archive work more user friendly

(defn- utc-now []
  #?(:cljs (.toUTCIsoString (gd/DateTime.))
     :clj (.toString (Instant/now))))

(defmulti value-serializer (juxt ::primary-key ::attribute))
(defmethod value-serializer :default [_] swark/->str)

(defmulti attribute-serializer ::primary-key)
(defmethod attribute-serializer :default [_] swark/->str)

(defmulti primary-value-serializer ::primary-key)
(defmethod primary-value-serializer :default [_] swark/->str)

(defmulti primary-key-serializer ::primary-key)
(defmethod primary-key-serializer :default [_] swark/->str)

(defn- unparse
  [row-item]
  (-> row-item
      (update ::primary-key (primary-key-serializer row-item))
      (update ::primary-value (primary-value-serializer row-item))
      (update ::attribute (attribute-serializer row-item))
      (update ::value (value-serializer row-item))
      (update ::flags #(when (seq %) (->> % (map swark/->str) (into []) str)))))

(def ^:private entry->row (juxt ::tx-utc-at ::primary-key ::primary-value ::attribute ::value ::flags))

(defn serialize
  [{:keys [primary-key flags]
    :or   {flags #{}}} item]
  (let [entity    (find item primary-key)
        tx-utc-at (utc-now)]
    (assert entity)
    (->> (dissoc item primary-key)
         (map (fn [[attribute value]]
                {::tx-utc-at     tx-utc-at
                 ::entity        entity
                 ::primary-key   primary-key
                 ::primary-value (get item primary-key)
                 ::attribute     attribute
                 ::value         value
                 ::flags         flags #_ {::deleted ::archived}}))
         (map unparse)
         (map entry->row))))

(defmulti value-parser (juxt ::primary-key ::attribute))
(defmethod value-parser :default [_] identity)

(defmulti attribute-parser ::primary-key)
(defmethod attribute-parser :default [_] keyword)

(defmulti primary-value-parser ::primary-key)
(defmethod primary-value-parser :default [_] identity)

(defmulti primary-key-parser ::primary-key)
(defmethod primary-key-parser :default [_] keyword)

(def ^:private row->entry
  (partial zipmap [::tx-utc-at ::primary-key ::primary-value ::attribute ::value ::flags]))

(defn- parse-flags
  [s]
  (let [coll (edn/read-string s)]
    (when (coll? coll)
      (->> coll
           (mapv (partial keyword (namespace ::this)))
           set))))

(defn- parse-entry [entry]
  (let [find-entity (juxt ::primary-key ::primary-value)
        entry       (as-> entry e
                      (update e ::primary-key (primary-key-parser e #_ntry))
                      (update e ::primary-value (primary-value-parser e #_ntry))
                      (update e ::attribute (attribute-parser e #_ntry))
                      (update e ::value (value-parser e #_ntry))
                      (update e ::flags parse-flags))
        entity      (find-entity entry)]
    (assoc entry ::entity entity)))

(comment
  (let [rows (serialize {:primary-key :id} {:id 123 :name "Stan" :test "ikel"})]
    (->> rows
         (map row->entry)
         (map parse-entry)))
  )

(defn- entry->map [{::keys [attribute value entity flags] :as entry}]
  {entity (into {attribute value} [entity]) #_entry})

(defn- merge-entries [map1 {::keys [attribute flags] :as map2}]
  (cond
    (some-> flags ::archived)
    nil

    (some-> flags ::deleted)
    (dissoc map1 attribute)

    :else
    (merge map1 map2)))

(defn- filter-entry [props entry]
  (if-not (seq props)
    true
    (let [props  (->> props (filter (comp ifn? val)) (into {}))
          keyseq (keys props)
          keys   (set/intersection (-> entry keys set) (-> props keys set))
          values (if (seq keys)
                   (apply juxt keys)
                   (fn [_] (vector nil)))]
      (some->> (select-keys entry keyseq)
               (merge-with #(%1 %2) props)
               values
               seq
               (every? identity)))))


(defn- filterer [props]
  (filter (partial filter-entry props)))

(defn merge-rows
  "Return eagerly parsed and merged rows"
  ([rows]
   (merge-rows nil rows))
  ([filter-props rows]
   (transduce
     (comp
       (map row->entry)
       (map parse-entry)
       (filterer filter-props)
       (map entry->map))
     (partial merge-with merge-entries)
     rows)))

(comment
  (let [rows (serialize {:primary-key :id} {:id 123 :name "Stan" :test "ikel"})]
    #_(map parse-entry (map row->entry rows))
    (merge-rows #_{::entity #{[:id 12]}} rows))
  )

(defn- diff [primary-key & items]
  (let [entity            (find (apply merge items) primary-key)
        [removed added _] (apply data/diff items)
        removed           (some->> removed
                                   (remove (comp (or added {}) key))
                                   seq
                                   (into {}))]
    {::added   added
     ::removed removed}))

(defn- diff-rows [{:keys [primary-key] :as props} & items]
  (let [entity            (select-keys (apply merge items) [primary-key])
        {::keys
         [added removed]} (apply diff primary-key items)]
    (concat
      (serialize (assoc props :flags #{::deleted}) (merge removed entity))
      (serialize props (merge added entity)))))

(defn- upsert-rows*
  [#_rows db-map {:keys [primary-key next-primary-val] :as props} item]
  (let [#_#_db-map (merge-rows {::primary-key #{primary-key}} rows)
        update?    (contains? item primary-key)
        next-pval  #(->> db-map keys (map second) set next-primary-val)
        item       (cond-> item
                     (not update?) (assoc primary-key (next-pval)))
        entity     (find item primary-key)]
    (if update?
      (diff-rows props (get db-map entity) item)
      (serialize props item))))

(defn upsert
  [rows {:keys [primary-key] :as props} & items]
  (when (seq items)
    (let [db-map (merge-rows {::primary-key #{primary-key}} rows)]
      (mapcat
        (partial upsert-rows* db-map props)
        items))))

(comment
  (upsert-rows {[:id "a"] {:id "a" :test "ikel"}} {:primary-key :id :next-primary-val swark/unid} {:test "ikeltje"})
  
  (upsert-rows [["abc" "id" "a" "test" "ikel" ""]] {:primary-key :id :next-primary-val swark/unid} {:test "ikeltje"})

  (diff-rows {:primary-key :id} {:id 0 :test "ikel"} {:id 0 :test "wat?"})

  (upsert-rows* [["abc" "id" "a" "test" "ikel" ""]
                 ["abc" "id" "a" "more" "nonsense" ""]]
                {:primary-key :id :next-primary-val swark/unid}
                {:id "a" :test "ikeltje" :more "eh"})

  (upsert [["abc" "id" "a" "test" "ikel" ""]
           ["abc" "id" "a" "more" "nonsense" ""]]
          {:primary-key :id :next-primary-val swark/unid}
          {:id "a" :test "ikeltje" :more "eh"}
          {:ohno "myg"})

  (let [txd  (utc-now) 
        rows [[txd "id" "a" "test" "ikel" ""]
              [txd "id" "a" "more" "nonsense" ""]]
        ]
    (->> rows
         (map row->entry)
         (map parse-entry)
         (filter (partial filter-entry {::primary-key #{:no}})))
    #_(merge-rows #_{:primary-key #{:id}} rows))

  )

