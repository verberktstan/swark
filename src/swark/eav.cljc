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

(defn parse-row
  "Returns row as a map with :entity/attribute, :entity/value, :attribute & :value. Applies supplied parsers on the fly."
  ([row]
   (parse-row nil row))
  ([props row]
   (let [keyseq [:entity/attribute :entity/value :attribute :value]]
     (->> row
          (zipmap keyseq)
          (merge-with parse (select-keys props keyseq))))))
