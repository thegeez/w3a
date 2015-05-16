(ns net.thegeez.w3a.binding
  (:require [io.pedestal.interceptor :as interceptor]
            [io.pedestal.impl.interceptor :as impl-interceptor]
            [io.pedestal.log :as log]))

(defn validate [binding context]
  (let [errors (reduce
                (fn [errors b]
                  (if-let [f (:validator b)]
                    (let [res (f context)]
                      (merge errors res))
                    errors))
                nil
                binding)
        values (into {}
                     (for [{:keys [id normalize] :as b} binding]
                       (if normalize
                         (normalize context)
                         [id (get-in context [:request :params id])])))]
    [values errors]))

(defn with-binding [binding-spec]
  (let [[binding-name binding] (first binding-spec)]
    (interceptor/interceptor
     {:name ::with-binding
      :enter (fn [context]
               (if-not (contains? #{:post :put} (get-in context [:request :request-method]))
                 context
                 (let [[values errors] (validate binding context)]
                   (if errors
                     (-> context
                         (update-in [:response]
                                    merge
                                    {:status 400
                                     :data {binding-name
                                            {:values values
                                             :errors errors}}})
                         impl-interceptor/terminate)
                     (assoc-in context [:request :values] values)))))
      :leave (fn [context]
               (update-in context [:response :data binding-name]
                          assoc ::binding binding))})))
