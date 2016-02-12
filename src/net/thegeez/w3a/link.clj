(ns net.thegeez.w3a.link
  (:require [clojure.string :as string]
            [io.pedestal.interceptor :as interceptor]
            [io.pedestal.http.route :as route]
            [io.pedestal.log :as log]))

(defn link [context kw-name & options]
  (let [linker (get-in context [:bindings #'route/*url-for*])
        {:keys [absolute? query-params]
         :or {absolute? true}} options
        [scheme port] (if (get-in context [:request :headers "x-forwarded-proto"])
                        [:https 443]
                        [:http nil])]
    (apply linker kw-name
           :absolute? absolute?
           :scheme scheme
           :port port
           ;; TODO also auto insert ?format= param based on current request
           :query-params query-params
           options)))

(defn self [context]
  (when-let [kw-name (get-in context [:route :route-name])]
    (link context kw-name :query-params (get-in context [:request :query-params]))))

(defn next-or-link [context link-name & opts]
  (let [next (get-in context [:request :query-params :next])]
    (if (and next
             (seq next))
      next
      (apply link context link-name opts))))

(defn link-with-next [context link-name & opts]
  (apply link context link-name (mapcat identity
                                        (if-let [next (get-in context [:request :params :next])]
                                          (assoc-in opts [:params :next] next)
                                          opts))))

(defmulti coerce-param (fn [type v]
                         type))

(defmethod coerce-param :long
  [_ v]
  (try
    (Long/parseLong v)
    (catch Exception _ nil)))

(defn coerce-path-params [name+type]
  (interceptor/interceptor
   {:name ::coerce-path-params
    :enter (fn [context]
             (update-in context [:request :path-params]
                        (fn [params]
                          (reduce-kv
                           (fn [m k v]
                             (if-let [type (get name+type k)]
                               (assoc m k (coerce-param type v))
                               (assoc m k v)))
                           {}
                           params))))}))
