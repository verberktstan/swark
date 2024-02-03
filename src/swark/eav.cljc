(ns swark.eav
  {:added "0.1.3"
   :doc "Serialize and parse data as entity-atteibute-value rows."}
  (:require [clojure.data :as data]
            [swark.core :refer [->str]]))

;; Storing data as Entity / Attribute / Value rows

(defn- parse [f input] (cond-> input (ifn? f) f))

;; TODO: Add test for append arguments
(defn ->rows
  "Returns a sequence of vectors with [entity-attribute entity-value attribute value]
  for each map-entry in map m."
  ([m] (->rows m nil))
  ([m {primary-key :primary/key
       parse-entity-attribute :entity/attribute
       parse-entity-value :entity/value
       parse-attribute :attribute
       parse-value     :value
       :or {primary-key            :id
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
(defn- diff [primary-key & maps]
  (let [entity (-> (apply merge maps) (find primary-key))
        [removed added _] (apply data/diff maps)
        removed (some->> removed
                         (remove (comp (or added {}) key))
                         seq
                         (into {}))]
    {::removed removed
     ::added   added}))

(defn diff-rows
  [{primary-key :primary/key :as props} & maps]
  (let [diff* (apply diff primary-key maps)
        entity (select-keys (apply merge maps) [primary-key])
        removed (-> diff* ::removed (merge entity))
        added   (-> diff* ::added (merge entity))]
    (concat
      (->rows removed props (->str ::removed))
      (->rows added props))))

(comment
  (diff-rows {:primary/key :id} {:id 0 :ab "cd" :cd "ef"} {:id 0 :ab "de"}) 
  )

(defn- assert-ifn-vals
  [props]
  (when (seq props)
    (doseq [[k f] props]
      (-> f ifn? (assert (str k " does not implement IFn!"))))))

(defn- parse-row
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

(defn parser
  [props]
  (map (partial parse-row props)))

(defn- filter-eav
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

(defn filterer
  [props]
  (filter (partial filter-eav props)))

(defn- mapify
  [{ea  :entity/attribute
    ev  :entity/value
    a   :attribute
    v   :value
    rm? ::removed}]
  {[ea ev] (cond-> {a v}
             rm?       (assoc ::removed true)
             (not rm?) (assoc ea ev))})

(defn- merge-rows* [map1 {::keys [removed] :as map2}]
  (if removed
    (apply dissoc map1 (keys map2))
    (merge map1 map2)))

(defn merge-rows
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
