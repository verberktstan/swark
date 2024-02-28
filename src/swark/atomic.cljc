(ns swark.atomic
  (:require [clojure.core.async :as a]
            [swark.core :as swark]))

;; TODO: Support channel transducers and ex-handler as well
(defn atomic
  {:added "0.1.41"
   :arglist '([x])
   :doc "Returns a map representin a connection to something stateful or with
   side-effects, so you can treat state or side-effects in a similar manner to
   Clojure atoms.
   Starts a go-loop in the background and returns a map with ::in and ::out async
   channels. Input to ::in chan is expected to be [f & args] or [::closed!]. In
   the latter case, the go-loop will stop. In the first case, (apply f x args)
   will be called and the result is put on ::out chan.
   Valid props are :in-buffer-size (default 99) & :out-buffer-size (default 99).
   An atomic thing is similar to an atom, you can `put!` things, just like you
   would swap! an atom. Be sure to close the atomic by calling `close!` on it."}
  [x & options]
  (let [props    (merge
                  {:in-buffer-size 99 :out-buffer-size 99}
                  (apply hash-map options))
        spec     {:in-buffer-size pos-int? :out-buffer-size pos-int?}
        _        (-> spec (swark/valid-map? props) assert)
        in-chan  (a/chan (a/sliding-buffer (:in-buffer-size props)))
        out-chan (a/chan (a/dropping-buffer (:out-buffer-size props)))]
    (a/go-loop [[f & args] (a/<! in-chan)]
      (when-not (some-> f #{::closed!}) ; NOTE: Stop the go-loop in this case
        (if-some [result (when (fn? f) (apply f x args))]
          (a/>! out-chan result)
          (a/>! out-chan ::nil)) ; Put explicit ::nil on channel
        (recur (a/<! in-chan))))
    {::in  in-chan
     ::out out-chan}))

(defn put!
  {:added "0.1.41"
   :arglist '([atomic & args])
   :doc "Put args on the ::in chan and block until something is returned via
   ::out chan. Returns the returned value.
   Similar to swap! for atoms, but implemented as a async messaging system, so
   it's useable for side-effects as well."}
  [{::keys [in out]} & args]
  (assert in)
  (assert out)
  (a/go (a/>! in (or (->> args (keep identity) seq) [::closed!]))) ; NOTE: Close the go-loop when nil args
  (let [result (a/<!! out)]
    (when-not (some-> result #{::nil}) ; Simply return nil when ::nil is returned
      result)))

(defn close!
  {:added "0.1.41"
   :arglist '([atomic])
   :doc "Stops the underlying go-loop and closes all channels. Returns nil."}
  [atomic]
  (-> atomic ::in assert)
  (a/>!! (::in atomic) [::closed!]) ; NOTE: Close the running go-loop
  (let [channels (juxt ::in ::out)]
    (run! a/close! (channels atomic)))
  ::closed!)
