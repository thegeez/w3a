(ns net.thegeez.w3a.form
  (:require [io.pedestal.interceptor :as interceptor]
            [net.thegeez.w3a.context :refer [combine]]))

(defn html-form [context opts]
  [:form {:action (:action opts)
          :method "POST"
          :accept-charset "UTF-8"}
   (when-let [method (:method opts)]
     [:input {:type "hidden"
              :name "_method"
              :value (name method)}])
   [:input {:type "hidden"
            :name "__utf-8"
            :value "✓"}]
   [:input {:type "hidden"
            :name "__anti-forgery-token"
            :value (get-in context [:request :io.pedestal.http.csrf/anti-forgery-token])}]
   (:fields opts)
   [:div.form-group
    [:input.btn.btn-primary
     {:type "submit"
      :value (get opts :submit "Submit")}]]])

(defmulti render-form-field
  (fn [{:keys [id label name type render] :as def} value errors]
    (or render
        type))
  :default :string)

(defmethod render-form-field :string
  [{:keys [id label name] :as def} value errors]
  [:div
   {:class (str "form-group"
                (when errors
                  " has-error"))}
   [:label.control-label
    {:for name} label]
   [:input.form-control
    {:type "text"
     :id name
     :name name
     :value value}]
   (when errors
     (for [error errors]
       [:span.help-block
        error]))])

(defmethod render-form-field :password
  [{:keys [id label name] :as def} value errors]
  [:div
   {:class (str "form-group"
                (when errors
                  " has-error"))}
   [:label.control-label
    {:for name} label]
   [:input.form-control
    {:type "password"
     :id name
     :name name
     :value value}]
   (when errors
     (for [error errors]
       [:span.help-block
        error]))])

(defmethod render-form-field :static
  [{:keys [id label name] :as def} value errors]
  [:div
   {:class (str "form-group"
                (when errors
                  " has-error"))}
   [:label.control-label
    {:for name} label]
   [:div
    [:p.form-control-static
     value]]
   (when errors
     (for [error errors]
       [:span.help-block
        error]))])

(defn form-fields [prefix form data]
  (for [field form]
    (let [{:keys [id label type render]} field
          name (str (name prefix) "[" (name id) "]")
          value (get data id)
          errors (get-in data [:errors id])
          field (assoc field :name name)]
      (render-form-field field value errors))))

(defmulti coerce-form-field (fn [value type]
                              type))

(defmethod coerce-form-field :long
  [value type]
  (try (Long/parseLong value)
       (catch Exception _ nil)))

(defmethod coerce-form-field :string
  [value type]
  value)

(defmethod coerce-form-field :default
  [value type]
  value)

(defn parse-form [prefix form]
  (interceptor/interceptor
   {:name ::parse-form
    :enter (fn [context]
             (let [data (get-in context [:request :params prefix])

                   ;; html form needs to be coerced
                   data (if (= (get-in context [:request :headers "content-type"])
                               "application/x-www-form-urlencoded")
                          (reduce
                           (fn [data {:keys [id type]}]
                             (update-in data [id] coerce-form-field type))
                           data
                           form)
                          data)

                   [errors values] (reduce
                                    (fn [[errors values]
                                         {:keys [id type validator coerce] :as input}]
                                      (let [value (get data id)
                                            error (when validator
                                                    (when-let [error (validator value)]
                                                      {id [error]}))
                                            value (if (= type :static)
                                                    nil
                                                    (if error
                                                      value
                                                      (if coerce
                                                        (coerce value)
                                                        value)))]
                                        [(merge errors error)
                                         (if value
                                           (assoc values id value)
                                           values)]))
                                    [{} {}]
                                    form)

                   data (merge values
                               (when (seq errors)
                                 {:errors errors}))]
               (combine context
                        {:request {:data {prefix data}}})))}))
