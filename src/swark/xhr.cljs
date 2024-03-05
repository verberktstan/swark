(ns  swark.xhr
  (:require [cljs.reader :as reader]))

;; Minimalistic xhr requets

;; Start repl from the commandline: clojure -M:repl/cljs
;; Connect from this namespace `cider-connect-cljs` (and select 'browser' when asked)
;; Open the browser js console
;; You're good to go!

(def METHODS {:get "GET" :post "POST" :put "PUT" :delete "DELETE"})

(defn- set-request-header [request k v]
  (cond-> request
    (and k v) (.setRequestHeader k v)))

(defn- set-response-type [request response-type]
  (if response-type
    (set! (.-reponseType request) response-type)
    request))

(def STATES [:unsent :opened :headers-received :loading :done])
(defn STATUSES [status]
  (cond
    (<= 100 status 199) :informational
    (<= 200 status 299) :success
    (<= 300 status 399) :redirection
    (<= 400 status 499) :client-error
    (<= 500 status 599) :server-error
    :else :generic-error))

(defn request
  [url on-success & args]
  (-> url string? assert)
  (-> on-success fn? assert)
  (let [{:keys [method content-type async? on-error user password]} (apply hash-map args)
        request (js/XMLHttpRequest.)
        method (get METHODS method "GET")
        async? (boolean async?)
        content-type (get {:edn  "application/clojure"
                           :clj  "application/clojure"
                           :json "application/json"
                           :csv  "text/csv"
                           :html "text/html"} content-type "text/plain")
        clj-response? (some-> content-type #{:edn :clj})
        response-type (when async?
                        (get {:json "json" :html "document"} content-type "text"))]
    (when on-success
      (set! (.-onreadystatechange request)
            #(when (and (-> request .-readyState STATES #{:done})
                        (-> request .-status STATUSES #{:success}))
               (on-success (cond-> (.-response request) clj-response? reader/read-string)))))
    (when on-error
      (set! (.-onreadystatechange request)
            #(when (and (-> request .-readyState STATES #{:done})
                        (-> request .-status STATUSES #{:client-error :server-error}))
               (on-error (cond-> (.-response request) clj-response? reader/read-string)))))
    (if (and user password)
      (.open request method url async? user password)
      (.open request method url async?))
    (doto request
      (set-request-header "Content-Type" content-type)
      (set-response-type response-type)
      (.send))
    (or (and async? request)
        (cond-> (.-response request) clj-response? reader/read-string))))

(comment
  (def atm (atom nil))
  @atm 

  (request
   "/README.md"
   #(swap! atm assoc :status :success :response %)
   :method       :get
   :content-type :html
   :async?       true
   :on-error      #(swap! atm assoc :status :error :response %))

  (request
   "/deps.edn"
   #(swap! atm assoc :status :success :response %)
   :method       :get
   :content-type :text
   :async?       false))
