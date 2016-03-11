(ns net.thegeez.w3a.edn
  (:require [io.pedestal.interceptor :as interceptor]))

(defn for-edn
  [template-fn]
  (interceptor/interceptor
   {:name ::for-edn
    :leave (fn [context]
             (if (and (.contains ^String (get-in context [:request :headers "accept"] "") "application/edn")
                      (not= "application/edn" (get-in context [:response :headers "Content-Type"])))
               (-> context
                   (assoc-in [:response :headers "Content-Type"] "application/edn")
                   (assoc-in [:response :body]
                             (let [body (template-fn context)]
                               (if (string? body)
                                 body
                                 (pr-str body)))))
               context))}))
