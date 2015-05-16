(ns net.thegeez.w3a.form
  (:require [io.pedestal.log :as log]))

(defmulti render-binding
  (fn [def value errors]
    (or (:widget def)
        (:type def))))

(defmethod render-binding :default
  [def value errors]
  (let [{:keys [id label type]} def
        field-name (or label (name id))]
    [:div
     {:class (str "form-group"
                  (when errors
                    " has-error"))}
     [:label.control-label
      {:for (name id)}
      field-name]
     [:div.input-group
      (cond
       (= type :string)
       [:input.form-control
        {:type "text"
         :name (name id)
         :id (name id)
         :value value}]
       (= type :password)
       [:input.form-control
        {:type "password"
         :name (name id)
         :id (name id)}]
       :else type)]
     (when errors
       [:div.errors
        (for [error errors]
          [:span.help-block error])])]))


(defn scaffold-form [context name]
  (let [{:keys [:net.thegeez.w3a.binding/binding] :as data} (get-in context [:response :data name])
        {:keys [values errors]} (get-in context [:request :response-state name])]
    [:div.panel.panel-default
     [:div.panel-body
      [:form {:method "POST"
              :action (str (get-in context [:self])
                           (when-let [return-to (get-in context [:request :query-params :return-to])]
                             (str "?return-to=" return-to)) )
              :class "form"}
       [:input {:type "hidden"
                :name "__anti-forgery-token"
                :value (get-in context [:request :io.pedestal.http.csrf/anti-forgery-token])}]
       [:fieldset
        (for [{:keys [id] :as def} binding]
          (let [value (or
                       (get values id)
                       (get data id))
                errors (get errors id)]
            (render-binding def value errors)))]
       [:div.form-actions
        [:input {:class "btn btn-primary"
                 :type "submit"
                 :value "Submit"}]]]]]))
