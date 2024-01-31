(ns swark.eav)

;; Storing data as Entity / Attribute / Value rows

(defn- parse [f input] (f input))

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
  "Returns row as a map with :entity/attribute, :entity/value, :attribute & :value. Applies supplied parsers on the fly."
  ([row]
   (parse-row nil row))
  ([props row]
   (let [keyseq [:entity/attribute :entity/value :attribute :value]
         props  (select-keys props keyseq)]
     (assert-ifn-vals props)
     (->> row
          (zipmap keyseq)
          (merge-with parse props))))
  ([props lookup row] ;TODO: Merge lookop into props?
    (let [m (parse-row props row)
          ea-parser (get lookup :entity/attribute identity)
          eaa (juxt (comp ea-parser :entity/attribute) :attribute)
          value-parser (get lookup (eaa m))]
      (cond-> m
        ea-parser (update:entity/attribute ea-parser)
        value-parser (update :value value-parser)))))

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
