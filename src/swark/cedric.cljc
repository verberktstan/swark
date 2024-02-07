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
;; TODO: Write rows to csv, and test with that.

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
        tx-utc-at (utc-now)
        archived? (some-> flags ::archived)
        item'     (if archived?
                    (select-keys item [primary-key])
                    (dissoc item primary-key))]
    (assert entity)
    (->> item'
         (map (fn [[attribute value]]
                {::tx-utc-at     tx-utc-at
                 ::entity        entity
                 ::primary-key   primary-key
                 ::primary-value (get item primary-key)
                 ::attribute     attribute
                 ::value         value
                 ::flags         (map name flags)}))
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

(defn- entry->map [{::keys [attribute value entity flags] :as entry}]
  {entity (with-meta (into {attribute value} [entity]) {::flags flags}) #_entry})

(defn- merge-entries [map1 {::keys [attribute] :as map2}]
  (some-> map2 meta ::flags println)
  (cond
    (some-> map2 meta ::flags ::archived)
    nil ; Return nil, this value is to be removed from the result later

    (some-> map2 meta ::flags ::deleted)
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
   (keep
     val ; Only keep vals of the db maps, and omit archived entries
     (transduce
       (comp
         (map row->entry)
         (map parse-entry)
         (filterer filter-props)
         (map entry->map))
       (partial merge-with merge-entries)
       rows))))

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

(defn- upsert-rows
  [db-items {:keys [primary-key next-primary-val]
             :or   {next-primary-val swark/unid}
             :as   props} item]
  (let [update?   (contains? item primary-key)
        next-pval #(->> db-items (map (fn [item] (get item primary-key))) set next-primary-val)
        item      (cond-> item
                    (not update?) (assoc primary-key (next-pval)))
        entity    (find item primary-key)]
    (if update?
      (diff-rows props (->> db-items (filter (comp #{entity} #(find % primary-key))) first) item)
      (serialize props item))))

(defn upsert
  [rows {:keys [primary-key] :as props} & items]
  (assert (seq items))
  (let [db-map (merge-rows {::primary-key #{primary-key}} rows)]
    (mapcat
      (partial upsert-rows db-map props)
      items)))

(defn archive
  [rows {:keys [primary-key] :as props} & items]
  (assert (and (seq items) (every? #(get % primary-key) items)))
  (mapcat (partial serialize (assoc props :flags #{::archived})) items))

(comment
  ;; Step 1 make this work in memory!
  (def DB (atom nil))
  (swap! DB (fn [db]
              (concat db (upsert db {:primary-key      :user/id
                                     :next-primary-val swark/unid} {:user/name "Antilla"} {:user/name "Ben Hur"}))))
  (merge-rows {::entity #{[:user/id "c"] [:user/id "09"]}} @DB)
  (swap! DB (fn [db]
              (concat db (upsert db {:primary-key :user/id} {:user/id "2" :user/name "VOID"}))))
  #_(upsert @DB {:primary-key      :user/id
                 :next-primary-val swark/unid} {:user/name "Antilla"} {:user/name "Ben Hur"})

  (swap! DB (fn [db]
              (concat db (archive db {:primary-key :user/id} {:user/id "bb"} {:user/id "f"}))))
  

  ;; Step 2 Make this work with csv
  )

;; Instead of CRUD, we have URA
;; Upsert = Create and Update
;; Read = Read
;; Archive = Delete

(defprotocol Cedric
  (upsert-items [this props items])
  (read-items [this props])
  (archive-items [this props items]))

(defrecord Mem [rows-atom]
  Cedric
  (upsert-items [this {:keys [primary-key] :as props} items]
    (->> (swap! (:rows-atom this) (fn [rows] (concat rows (apply upsert rows props items))))
         (merge-rows {::primary-key #{primary-key}})))
  (read-items [this props] (merge-rows props (-> this :rows-atom deref)))
  (archive-items [this {:keys [primary-key] :as props} items]
    (swap! (:rows-atom this) (fn [rows] (concat rows (apply archive rows props items))))
    {::archived (count items)}))

(comment

  (def CM (Mem. (atom nil)))

  (upsert-items CM {:primary-key :user/id} [{:user/name "Stan"} {:user/name "Corinne"} {:user/name "David"}])
  ;; (read-items CM {::entity #{[:user/id "9"]}})
  (read-items CM {})
  (archive-items CM {:primary-key :user/id} [{:user/id "1"} {:user/id "4"}])
)

