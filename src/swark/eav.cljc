(ns swark.eav)

;; Storing data as Entity / Attribute / Value rows

(defn ->rows
  "Returns a sequence of vectors with [entity-attribute entity-value attribute value]
  for each map-entry in map m."
  [primary-key m]
  (let [entry (find m primary-key)]
    (assert entry "Mapentry can't be found!")
    (map (partial into entry) (dissoc m primary-key))))

(defn- parse [f input] (f input))

(defn- parse-row
  "Returns row as a map with :entity/attribute, :entity/value, :attribute & :value. Applies supplied parsers on the fly."
  ([row]
   (parse-row nil row))
  ([props row]
   (let [keyseq [:entity/attribute :entity/value :attribute :value]]
     (->> row
          (zipmap keyseq)
          (merge-with parse (select-keys props keyseq))))))

(defn parser
  [props]
  (map (partial parse-row props)))

(defn- filter-eav
  [props eav-map]
  (let [keyseq (keys props)
        props' (select-keys props (keys eav-map))
        filter-vals (apply juxt keyseq)]
    (if-not (seq props')
      eav-map
      (->> (select-keys eav-map keyseq)
        (merge-with parse props')
        filter-vals
        (every? identity)))))

(defn filterer
  [props]
  (filter (partial filter-eav props)))

(defn mapify
  [{ea :entity/attribute
    ev :entity/value
    a  :attribute
    v  :value}]
  {[ea ev] a v})

(defn merge-rows
  [parse-props filter- rows]
  (transduce
    (comp (parser parse-props)
          (filterer filter-props)
          mapify)
    (partial merge-with merge)
    rows))