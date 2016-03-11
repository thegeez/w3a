(ns net.thegeez.w3a.oauth.google
  (:require [clj-http.client :as client]
            [environ.core :as environ]
            [io.pedestal.interceptor :as interceptor]
            [io.pedestal.impl.interceptor :as impl-interceptor]
            [io.pedestal.log :as log]
            [net.thegeez.w3a.link :as link]
            [ring.util.codec :as ring-codec])
  (:import [java.math BigInteger]
           [java.security SecureRandom]))

;; h/t https://github.com/propan/geheimtur

(def google-auth-url "https://accounts.google.com/o/oauth2/auth")
(def google-token-url "https://www.googleapis.com/oauth2/v3/token")
(def google-user-info-url "https://www.googleapis.com/oauth2/v1/userinfo")

(defn get-client-settings []
  (let [client-id (get environ/env :google-client-id)
        client-secret (get environ/env :google-client-secret)
        redirect-uri (get environ/env :google-redirect-uri)]
    (assert (and client-id client-secret redirect-uri)
            "Google oauth setting not found in env")
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
                     :redirect_uri  redirect-uri
                     :grant_type    "authorization_code"}]
    (try
      (let [response (client/post google-token-url {:form-params form-params
                                                    :throw-entire-message? true
                                                    :as :auto})]
        (when (client/success? response)
          ;; response is like
          ;; {:body {:access_token "<code>",
          ;;         :scope "",
          ;;         :token_type "bearer"}
          (:body response)))
      (catch Exception ex
        (log/warn :msg "Could not fetch OAuth access token from Google"
                  :exception ex)
        nil))))

(defn fetch-user-info
  [access-token]
  (try
    (let [response (client/get google-user-info-url {:query-params {:access_token access-token
                                                              :alt "json"}
                                                     :throw-entire-message? true
                                                     :as :auto})]
      (:body response))
    (catch Exception ex
      (log/warn :msg "Could not fetch user details from Google"
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
                                 :scope "profile"
                                 :state state}
                   url (create-url google-auth-url query-params)]
               (merge context
                      {:response
                       {:status 303
                        :headers {"Location" url}
                        :session
                        {:oauth-callback
                         {:orig-state state
                          :next (get-in context [:request :query-params :next])}}}})))}))

(defn not-allowed [context]
  (impl-interceptor/terminate
   (merge context
          {:response {:status 401
                      :body "not allowed"}})))

(defn callback [find-or-create-by-google-id]
  (interceptor/interceptor
   {:leave (fn [context]
             (let [{:keys [state code]} (get-in context [:request :query-params])
                   {:keys [next orig-state]} (get-in context [:request :session :oauth-callback])]
               (if (and state code next orig-state
                        (= state orig-state))
                 (if-let [user-info (when-let [token (fetch-token code)]
                                      (resolve-user-info token))]
                   ;; user-info {:access-token "<code>"
                   ;;            :identity {:name "First Last"
                   ;;                       :id "<some id>"}} 
                   (let [google-id (get-in user-info [:identity :id])
                         google-name (get-in user-info [:identity :name])
                         access-token (get-in user-info [:access-token])
                         {:keys [id] :as user} (find-or-create-by-google-id context google-id)]
                     (merge context
                            {:response
                             {:status 303
                              :headers {"Location" next}
                              :session {:auth {:id id}
                                        :suggested-name google-name}
                              :flash {:message "Logged in through Google"}
                              }}))
                   (not-allowed context))
                 (not-allowed context))))}))
