(ns swark.cedric2)

(defn- row->entry
  ;; TODO: Add TX to row
  ;; TODO: Add flags to row
  ;; TODO: Add custom parsing of eval and value
  "Returns an entry based on the supplied row. Entry is a map with entity
  associated with some key/values, eg `{[:id 1] {:id 1 :name 'Me'}}`"
  [[ea ev attribute value :as row] & {:keys [keywordize-attributes? ev-parser]}]
  (->> row (every? string?) assert)
  {[(cond-> ea keywordize-attributes? keyword) (cond-> ev ev-parser ev-parser)]
   {(cond-> attribute keywordize-attributes? keyword) value}})

(defn- merge-rows
  "Eagerly transform rows of eav data into a map of records keyed by entity.
  `props` may contain :keywordize-attributes? & :ev-parser.
  :keywordize-attributes? transforms all entity-attribute and attribute strings
  into a keyword. eg `id => :id` or `user/name => :user/name`
  :ev-parser is used to parse the entity-value. For example
  clojure.edn/read-string would turn `[:id \"1\"] => [:id 1]`"
  ([rows] (merge-rows nil rows))
  ([props rows]
   (let [props-list (mapcat identity props)]
     (transduce
       (map #(apply row->entry % props-list))
      (partial merge-with merge)
      rows))))
