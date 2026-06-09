(ns spec.swark.core-spec
  (:require [clojure.spec.alpha :as s]
            [swark.core :as core]))

;; Spec for the transducer version of `key-by`
(s/def ::key-by-transducer (s/fspec :args (s/cat :f ifn?) :ret fn?))

;; Spec for the two-argument version of `key-by`
(s/def ::key-by-2arg (s/fspec :args (s/cat :f ifn? :coll coll?) :ret map?))

;; Spec for the three-argument version of `key-by`
(s/def ::key-by-3arg (s/fspec :args (s/cat :a associative? :f ifn? :coll coll?) :ret ::key-by-result))

;; Multi-spec for `key-by`
(s/def ::key-by (s/multi-spec core/key-by :arglists))
(s/def :swark.core/key-by-1arg ::key-by-transducer)
(s/def :swark.core/key-by-2arg ::key-by-2arg)
(s/def :swark.core/key-by-3arg ::key-by-3arg)

;; Instrument in development
#?(:clj
   (when (System/getProperty "dev")
     (s/instrument 'swark.core/key-by)))