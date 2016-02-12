(ns net.thegeez.w3a.breadcrumb
  (:require [io.pedestal.interceptor :as interceptor]
            [net.cgrand.enlive-html :as enlive]
            [net.thegeez.w3a.link :as link]))

(defn add-breadcrumb
  ([name route-name]
     (add-breadcrumb name route-name {}))
  ([name route-name params+paths]
     (interceptor/interceptor
      {:name ::add-breadcrumb
       :enter (fn [context]
                (update-in context [:breadcrumbs]
                           (fnil conj [])
                           [name (link/link context route-name :params (zipmap (keys params+paths)
                                                                               (map (partial get-in context) (vals params+paths))))]))})))

(defn html [breadcrumbs]
  (enlive/html [:ul.breadcrumb
                (for [[title href] breadcrumbs]
                  [:li [:a {:href href} title]])]))
