(ns net.thegeez.w3a.link
  (:require [clojure.string :as string]
            [io.pedestal.http.route :as route]
            [io.pedestal.http.route.definition :as definition]
            [io.pedestal.interceptor :as interceptor]
            [io.pedestal.impl.interceptor :as impl-interceptor]
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
           :query-params (cond-> query-params
                                 (= (get-in context [:request :headers "accept"]) "application/edn+html")
                                 (assoc :format "dev"))
           options)))

(defn self [context]
  (when-let [kw-name (get-in context [:route :route-name])]
    (link context kw-name)))

(defn return-to-or-link [context link-name & opts]
  (let [return-to (get-in context [:request :query-params :return-to])]
    (if (and return-to
             (seq return-to))
      return-to
      (apply link context link-name opts))))

(defn add-breadcrumb [context title route-name]
  (update-in context [:response ::breadcrumbs]
                        (fnil conj [])
                        {:title title
                         :route-name route-name
                         :link (link context route-name :path-params (get-in context [:request :path-params]))}))

(defn breadcrumb [title route-name]
  (interceptor/interceptor
   {:name ::breadcrumb
    :enter (fn [context]
             (add-breadcrumb context title route-name))}))
