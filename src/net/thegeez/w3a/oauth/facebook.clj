(ns net.thegeez.w3a.oauth.facebook
  (:require [clj-http.client :as client]
            [environ.core :as environ]
            [io.pedestal.interceptor :as interceptor]
            [io.pedestal.impl.interceptor :as impl-interceptor]
            [io.pedestal.log :as log]
            [net.thegeez.w3a.interceptor :refer [combine]]
            [net.thegeez.w3a.link :as link]
            [net.thegeez.gatherlist.data.users :as users]
            [ring.util.codec :as ring-codec])
  (:import [java.math BigInteger]
           [java.security SecureRandom]))

;; h/t https://github.com/propan/geheimtur

(def facebook-auth-url "https://www.facebook.com/dialog/oauth")
(def facebook-token-url "https://graph.facebook.com/v2.3/oauth/access_token")
(def facebook-user-info-url "https://graph.facebook.com/me")

(defn get-client-settings []
  (let [client-id (get environ/env :facebook-client-id)
        client-secret (get environ/env :facebook-client-secret)
        redirect-uri (get environ/env :facebook-redirect-uri)]
    (assert (and client-id client-secret redirect-uri)
            "Facebook oauth setting not found in env")
    {:client-id client-id
     :client-secret client-secret
     :redirect-uri redirect-uri}))

(defn create-afs-token
  "Creates a random state token to prevent request forgery."
  []
  (.toString (BigInteger. 130 (SecureRandom.)) 32))

(defn create-url
  "Creates a URL from a url string and a map with query parameters."
  [url query]
  (->> query
       ring-codec/form-encode
       (str url "?")))

(defn fetch-token
  "Fetches an OAuth access token using the given code and provider's configuration."
  [code]
  (let [{:keys [client-id client-secret redirect-uri]} (get-client-settings)
        form-params {:code          code
                     :client_id     client-id
                     :client_secret client-secret
                     :redirect_uri  redirect-uri}]
    (try
      (let [response (client/post facebook-token-url {:form-params form-params
                                                      :throw-entire-message? true
                                                      :as :auto})]
        (when (client/success? response)
          ;; response is like
          ;; {:body {:access_token "<code>",
          ;;         :scope "",
          ;;         :token_type "bearer"}
          (:body response)))
      (catch Exception ex
        (log/info :msg "Could not fetch OAuth access token from Facebook"
                  :exception ex)
        nil))))

(defn fetch-user-info
  [access-token]
  (try
    (let [response (client/get facebook-user-info-url {:oauth-token access-token
                                                       :throw-entire-message? true
                                                       :as :json})]
      (:body response))
    (catch Exception ex
      (log/info :msg "Could not fetch user details from Facebook"
                :exception ex)
      nil)))


(defn resolve-user-info
  [{:keys [access_token] :as token}]
  (when access_token
    (when-let [user-info (fetch-user-info access_token)]
      {:access-token access_token
       :identity user-info})))

(def authenticate
  (interceptor/interceptor
   {:leave (fn [context]
             (let [{:keys [client-id client-secret redirect-uri]} (get-client-settings)
                   state (create-afs-token)
                   query-params {:response_type "code"
                                 :client_id client-id
                                 :redirect_uri redirect-uri
                                 :scope "public_profile"
                                 :state state}
                   url (create-url facebook-auth-url query-params)]
               (combine context
                              {:response
                               {:status 303
                                :headers {"Location" url}
                                :session
                                {:oauth-callback
                                 {:orig-state state
                                  :return-to (get-in context [:request :query-params :return-to])}}}})))}))

(defn not-allowed [context]
  (impl-interceptor/terminate
   (combine context
            {:response {:status 401
                        :body "not allowed"}})))

(defn callback [find-or-create-by-facebook-id]
  (interceptor/interceptor
   {:leave (fn [context]
             (let [{:keys [state code]} (get-in context [:request :query-params])
                   {:keys [return-to orig-state]} (get-in context [:request :session :oauth-callback])]
               (if (and state code return-to orig-state
                        (= state orig-state))
                 (do
                   (log/info :check :ok)
                   (if-let [user-info (when-let [token (fetch-token code)]
                                        (resolve-user-info token))]
                    ;; user-info {:access-token "<code>"
                    ;;            :identity {:name "First Last"
                    ;;                       :id "<some id>"}}
                    (let [facebook-id (get-in user-info [:identity :id])
                          facebook-name (get-in user-info [:identity :name])
                          access-token (get-in user-info [:access-token])
                          {:keys [id name] :as user} (find-or-create-by-facebook-id context facebook-id)]
                      (if name
                        (combine context
                                 {:response
                                  {:status 303
                                   :headers {"Location" return-to}
                                   :session {:auth {:id id}}
                                   :flash {:message "Logged in through Facebook"}
                                   }})
                        (combine context
                                 {:response
                                  {:status 303
                                   :headers {"Location" (link/link context :oauth-create-name :params {:return-to return-to
                                                                                                       :suggested-name facebook-name})}
                                   :session {:auth-create-name {:id id}}}})))
                    (do (log/info :facebook-user-info :failed)
                        (not-allowed context))))
                 (do (log/info :facebook-failed [state code return-to orig-state])
                     (not-allowed context)))))}))
