(ns net.thegeez.w3a.oauth.github
  (:require [clj-http.client :as client]
            [environ.core :as environ]
            [io.pedestal.interceptor :as interceptor]
            [io.pedestal.impl.interceptor :as impl-interceptor]
            [io.pedestal.log :as log]
            [net.thegeez.w3a.interceptor :refer [combine]]
            [net.thegeez.w3a.link :as link]
            [ring.util.codec :as ring-codec])
  (:import [java.math BigInteger]
           [java.security SecureRandom]))

;; h/t https://github.com/propan/geheimtur


;; to check if user is already logged in at github: make a request
;; with the access token (from session or db) and see that it
;; succeeds, if it fails go through the whole thing again

(def github-auth-url "https://github.com/login/oauth/authorize")
(def github-token-url "https://github.com/login/oauth/access_token")
(def github-user-info-url "https://api.github.com/user")

(defn get-client-settings []
  (let [client-id (get environ/env :github-client-id)
        client-secret (get environ/env :github-client-secret)]
    (assert (and client-id client-secret)
            "Github oauth setting not found in env")
    {:client-id client-id
     :client-secret client-secret}))

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
  (let [{:keys [client-id client-secret]} (get-client-settings)
        form-params {:code          code
                     :client_id     client-id
                     :client_secret client-secret
                     :grant_type    "authorization_code"}]
    (try
      (let [response (client/post github-token-url {:form-params form-params
                                                    :throw-entire-message? true
                                                    :as :auto})]
        (when (client/success? response)
          ;; response is like
          ;; {:body {:access_token "<code>",
          ;;         :scope "",
          ;;         :token_type "bearer"}
          (:body response)))
      (catch Exception ex
        (log/warn :msg "Could not fetch OAuth access token from GitHub"
                  :exception ex)
        nil))))

(defn fetch-user-info
  [access-token]
  (try
    (let [response (client/get github-user-info-url {:oauth-token access-token
                                                     :throw-entire-message? true
                                                     :as :auto})]
      (:body response))
    (catch Exception ex
      (log/warn :msg "Could not fetch user details from GitHub"
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
             (let [{:keys [client-id client-secret]} (get-client-settings)
                   state (create-afs-token)
                   query-params {:client_id client-id
                                 ;; :scope  scope ;; default is read-only profile info
                                 :state state}
                   url (create-url github-auth-url query-params)]
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

(defn callback [find-or-create-by-github-login]
  (interceptor/interceptor
   {:leave (fn [context]
             (let [{:keys [state code]} (get-in context [:request :query-params])
                   {:keys [return-to orig-state]} (get-in context [:request :session :oauth-callback])]
               (if (and state code return-to orig-state
                        (= state orig-state))
                 (if-let [user-info (when-let [token (fetch-token code)]
                                      (resolve-user-info token))]
                   ;; user-info {:access-token "<code>"
                   ;;            :identity {:login "login nick"}}
                   ;; TODO set session to keep this info in session
                   (let [github-login (get-in user-info [:identity :login])
                         github-name (get-in user-info [:identity :name])
                         access-token (get-in user-info [:access-token])
                         {:keys [id name] :as user} (find-or-create-by-github-login context github-login)]
                     (if name
                       (combine context
                            {:response
                             {:status 303
                              :headers {"Location" return-to}
                              :session {:auth {:id id}}
                              :flash {:message "Logged in through GitHub"}
                              ;; :data {:context context
                              ;;        :oauth {:state state
                              ;;                :orig-state orig-state
                              ;;                :code code
                              ;;                :return-to return-to
                              ;;                :user-info user-info}}
                              }})
                       (combine context
                                {:response
                                 {:status 303
                                  :headers {"Location" (link/link context :oauth-create-name :params {:return-to return-to
                                                                                                      :suggested-name github-name})}
                                  :session {:auth-create-name {:id id}}}})))
                   (not-allowed context))
                 (not-allowed context))))}))
