(ns swark.cedric
  {:added "0.1.4"
   :doc "Protocol for persisting data as data driven EAV rows."}
  (:require [swark.core :as swark]
            [clojure.edn :as edn]
            [clojure.set :as set]
            [clojure.data :as data]
            #?(:cljs [goog.date :as gd])
            #?(:clj [clojure.java.io :as io])
            [clojure.data.csv :as csv])
  #?(:clj (:import [java.time Instant])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; CEDRIC - the Cedric Event DRIven datapersistence Companion
;; Store associatve data (maps) as rows in an append-only EAV database.

;; TODO: Test in cljs as well
;; TODO: Move back in time by filtering on txd (transaction's utc date)
;; TODO: Add some memoization with swark.core/memoire
;; TODO: Implement joins via join-rows, always one to many?

(defn- utc-now []
  #?(:cljs (.toUTCIsoString (gd/DateTime.))
     :clj (.toString (Instant/now))))

;; NOTE: These serializers are extensible, define your own methods for your database.
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

(defn- serialize
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

(defn- merge-rows
  "Return eagerly parsed and merged rows"
  ([rows]
   (merge-rows nil rows))
  ;; TODO: Make it possible to return only the items from the last tx. Or a specific tx?
  ([{:keys [post-merge-parser where]
     :or {post-merge-parser identity
          where identity}
     :as props} rows]
   (keep
    (comp #(when (where %) %) post-merge-parser val) ; Only keep vals of the db maps, and omit archived entries
    (transduce
     (comp
      (map row->entry)
      (map parse-entry)
      (filterer props)
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
        next-pval #(->> db-items
                        (map (fn [item] (get item primary-key))) ; NOTE: primary-key doesn't have to be a keyword!
                        set
                        next-primary-val)
        item      (cond-> item
                    (not update?) (assoc primary-key (next-pval)))
        entity    (find item primary-key)]
    (if update?
      (diff-rows props (->> db-items (filter (comp #{entity} #(find % primary-key))) first) item)
      (serialize props item))))

(defn- upsert
  [rows {:keys [pre-upsert-serializer primary-key]
         :or {pre-upsert-serializer identity}
         :as props} & items]
  (assert (seq items))
  (let [primary-values (fn [items] (->> items (map #(get % primary-key)) set))
        db-items (merge-rows {::primary-key #{primary-key}} rows)]
    (reduce
     (fn [new-rows item]
        ;; Take new rows from the other upserted items into account as well (for next-primary-val fn)
       (let [db-items' (concat db-items (merge-rows {} new-rows))]
         (concat new-rows (upsert-rows db-items' props item))))
     nil
     (map pre-upsert-serializer items))))

(defn- archive
  [rows {:keys [primary-key] :as props} & items]
  (assert (and (seq items) (every? #(get % primary-key) items)))
  (mapcat (partial serialize (assoc props :flags #{::archived})) items))

;; Instead of CRUD, we have URA
;; Upsert = Create and Update
;; Read = Read
;; Archive = Delete

(defprotocol Cedric
  (upsert-items [this props items] "Creates or updates the items, returning the items (with primary-key if created).")
  (find-by-entity [this entity] "Finds and returns one db entry, based off one specific entity.")
  (find-by-primary-key [this predicate props] "Finds and returns db entries where primary-key matches predicate, with additional props like {:where (comp #{'aname'} :name)}")
  (read-items [this props] "Returns all items with low level selectors in props, like ::primary-key, ::primary-value, ::entity, ::attribute, ::value and :where.")
  (archive-items [this props items] "Marks the items as archived in the db. Returns {::archived 2} where 2 is the item count actually archived."))

(defrecord Mem [rows-atom]
  Cedric
  (upsert-items [this {:keys [primary-key] :as props} items]
    ;; TODO: Return only the items from this (the last) tx
    (let [updated-pvals (seq (keep #(get % primary-key) items))]
      (->> (swap! rows-atom (fn [rows] (concat rows (apply upsert rows props items))))
           (merge-rows (cond-> {::primary-key #{primary-key}}
                         updated-pvals (assoc ::primary-value (set updated-pvals)))))))
  (find-by-entity [this entity]
    (-> this (read-items {::entity #{entity}}) first))
  (find-by-primary-key [this predicate props]
    (-> this (read-items (merge props {::primary-key predicate}))))
  (read-items [this props] (merge-rows props @rows-atom))
  (archive-items [this {:keys [primary-key] :as props} items]
    (swap! rows-atom (fn [rows] (concat rows (apply archive rows props items))))
    {::archived (count items)}))

(defn- write-csv! [filename rows]
  (with-open [writer (io/writer filename :append true)]
    (csv/write-csv writer rows :separator \;)))

(defn- open-or-create! [filename]
  (loop [reader (swark/jab io/reader filename)
         retries-left 3]
    (if (or reader (zero? retries-left))
      reader
      (do
        (write-csv! filename [])
        (recur (swark/jab io/reader filename) (dec retries-left))))))

(defn- read-csv [filename]
  (with-open [reader (open-or-create! filename)]
    (-> reader (csv/read-csv :separator \;) doall)))

#?(:clj
   (defrecord Csv [filename]
     Cedric
     (upsert-items [this {:keys [primary-key] :as props} items]
       (let [rows (read-csv filename)
             new-rows (apply upsert rows props items)
             updated-pvals (seq (keep #(get % primary-key) items))]
         (write-csv! filename new-rows)
         (merge-rows
          (cond-> {::primary-key #{primary-key}}
            updated-pvals (assoc ::primary-value (set updated-pvals)))
          new-rows)))
     (find-by-entity [this entity]
       (-> this (read-items {::entity #{entity}}) first))
     (find-by-primary-key [this predicate props]
       (-> this (read-items (merge props {::primary-key predicate}))))
     (read-items [this props] (merge-rows props (read-csv filename)))
     (archive-items [this props items]
       (let [rows (read-csv filename)
             new-rows (apply archive rows props items)]
         (write-csv! filename new-rows)
         {::archived (count new-rows)}))))

(comment
  ;; When using Csv implementation
  (def db-connection (swark/with-buffer (Csv. "/tmp/testdb.csv")))

  (swark/put! db-connection upsert-items {:primary-key :id} [{:test 123 :more "stuff"}])
  (swark/put! db-connection read-items {})

  (swark/close! db-connection))
