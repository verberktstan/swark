(ns swark.eav
  {:added "0.1.3"
   :doc "Serialize and parse data as entity-atteibute-value rows."})

;; Storing data as Entity / Attribute / Value rows

(defn- parse [f input] (cond-> input (ifn? f) f))

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
            (map (partial into entry)))))))

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
   (let [keyseq [:entity/attribute :entity/value :attribute :value]
         props     (select-keys props keyseq)
         _         (assert-ifn-vals props)
         item      (->> row
                     (zipmap keyseq)
                     (merge-with parse props))
         ea-vector (juxt :entity/attribute :attribute)
         v-parser  (get parsers (ea-vector item))]
     (cond-> item v-parser (update :value v-parser)))))

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
  [{ea :entity/attribute
    ev :entity/value
    a  :attribute
    v  :value}]
  {[ea ev] {ea ev a v}})

(defn merge-rows
  [parse-props filter-props rows]
  (transduce
   (comp
    (parser parse-props)
    (filterer filter-props)
    (map mapify))
   (partial merge-with merge)
   rows))
