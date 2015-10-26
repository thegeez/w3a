(ns net.thegeez.w3a.dev-interceptors
  (:require [clojure.string :as string]
            [io.pedestal.http :as http]
            [io.pedestal.interceptor :as interceptor]))

(defn wrap-dev-cljs [match replace]
  (interceptor/interceptor
   {:leave (fn [context]
             (cond-> context
                     (= (get-in context [:response :headers "Content-Type"]) "text/html")
                     (update-in [:response :body]
                                (fn [body]
                                  (when body
                                    (string/replace body match replace))))))}))

(defn wrap-cljs-interceptor [service match replace]
  (update-in service [::http/interceptors]
             (fn [ics]
               (into [(wrap-dev-cljs match replace)] ics))))
