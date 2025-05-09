(ns swark.cedric2)

(defn- row->entry
  ;; TODO: Add TX to row
  ;; TODO: Add flags to row
  ;; TODO: Add custom parsing of eval and value
  "Returns an entry based on the supplied row. Entry is a map with entity
  associated with some key/values, eg `{[:id 1] {:name 'Me'}}`"
  [[ea ev attribute value :as row] & {:keys [keywordize-attributes? ev-parser]}]
  (->> row (every? string?) assert)
  {[(cond-> ea keywordize-attributes? keyword) (cond-> ev ev-parser ev-parser)]
   {(cond-> attribute keywordize-attributes? keyword) value}})

(defn- merge-entity
  "Returns `record-value` with map-entry `entity-key` merged into it."
  [[entity-key record-value]]
  (or (when (and (vector? entity-key)
                 (map? record-value))
        (apply assoc record-value entity-key))
      record-value))

(defn- ->set
  "Returns a set of the map's values (like vals). Merges the entity entry (keys
  of map) into the record values."
  [record-map]
  (some->> record-map seq (map merge-entity) set))

(defn- some-matching-entities
  "Returns a predicate function that chesk if `entry-map` matches one of the
  map-entries in the supplied `records`."
  [records]
  (if-let [all-entities (->> records (mapcat seq) seq)]
    (fn  matching-entity* [entry-map]
      (when-let [entity (some-> entry-map keys first)]
        (some (partial = entity) all-entities)))
    (constantly true)))

(seq (mapcat seq [{:test "ikel"}]))

(defn- merge-rows
  "Eagerly transform rows of eav data into a map of records keyed by entity.
  `props` may contain :keywordize-attributes? & :ev-parser.
  :keywordize-attributes? transforms all entity-attribute and attribute strings
  into a keyword. eg `id => :id` or `user/name => :user/name`
  :ev-parser is used to parse the entity-value. For example
  clojure.edn/read-string would turn `[:id \"1\"] => [:id 1]`"
  ([rows] (merge-rows nil rows))
  ([{:keys [entities] :as props} rows]
   (let [props-list (mapcat identity props)]
     (transduce
      (comp
        (map #(apply row->entry % props-list))
        (filter (some-matching-entities entities)))
      (partial merge-with merge)
      rows))))
