(ns swark.cedric
  (:require [swark.core :as swark]
            [clojure.edn :as edn]
            [clojure.set :as set]
            #?(:cljs [goog.date :as gd])
            #?(:clj [clojure.string :as str]))
  #?(:clj (:import [java.time Instant])))

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
      (update ::flags #(when (seq %) (str (into [] (map swark/->str %)))))))

(def ^:private entry->row (juxt ::tx-utc-at ::primary-key ::primary-value ::attribute ::value ::flags))

(defn serialize
  [{:keys [primary-key]} item]
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
                 ::flags         #{} #_ {::deleted ::archived}}))
         (map unparse)
         (map entry->row))))

(defmulti value-parser (juxt ::primary-key ::attribute))
(defmethod value-parser :default [_] identity)

(defmulti attribute-parser ::primary-key)
(defmethod attribute-parser :default [_] keyword)

(defmulti primary-value-parser ::primary-key)
(defmethod primary-value-parser :default [_] edn/read-string)

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
