(ns net.thegeez.w3a.helpers
  (:require [io.pedestal.interceptor :as interceptor]
            [net.thegeez.w3a.edn-wrap :as edn-wrap]))

(defn for-html-interceptor [f]
  (interceptor/interceptor
   {:leave (fn [context]
             (cond-> context
                     (edn-wrap/for-html? context)
                     (->
                      (assoc-in [:response :headers "Content-Type"] "text/html")
                      (assoc-in [:response :body]
                                (f context)))))}))
