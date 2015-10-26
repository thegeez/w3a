(ns net.thegeez.w3a.oauth.twitter
  (:require [clj-http.client :as client]
            [environ.core :as environ]
            [io.pedestal.interceptor :as interceptor]
            [io.pedestal.impl.interceptor :as impl-interceptor]
            [io.pedestal.log :as log]
            [net.thegeez.w3a.interceptor :refer [combine]]
            [net.thegeez.w3a.link :as link]
            [ring.util.codec :as ring-codec]
            [oauth.client :as oauth])
  (:import [java.math BigInteger]
           [java.security SecureRandom]))

;; h/t https://github.com/mattrepl/clj-oauth

(def twitter-authorize-url "https://api.twitter.com/oauth/authenticate")
(def twitter-access-token-url "https://api.twitter.com/oauth/access_token")
(def twitter-request-token-url "https://api.twitter.com/oauth/request_token")

(defn get-client-settings []
  (let [client-id (get environ/env :twitter-client-id)
        client-secret (get environ/env :twitter-client-secret)
        redirect-uri (get environ/env :twitter-redirect-uri)]
    (assert (and client-id client-secret redirect-uri)
            "Twitter oauth setting not found in env")
    {:client-id client-id
     :client-secret client-secret
     :redirect-uri redirect-uri}))

(def consumer* (promise))

(defn get-consumer []
  (if (realized? consumer*)
    @consumer*
    (do (deliver consumer*
                 (let [{:keys [client-id client-secret]} (get-client-settings)]
                   (oauth/make-consumer client-id
                                        client-secret
                                        twitter-request-token-url
                                        twitter-access-token-url
                                        twitter-authorize-url
                                        :hmac-sha1)))
        @consumer*)))

(def authenticate
  (interceptor/interceptor
   {:leave (fn [context]
             (let [{:keys [redirect-uri]} (get-client-settings)
                   ;; request-token makes an http request to twitter
                   consumer (get-consumer)
                   request-token (oauth/request-token consumer redirect-uri)
                   url (oauth/user-approval-uri consumer (:oauth_token request-token))]
               (combine context
                              {:response
                               {:status 303
                                :headers {"Location" url}
                                :session
                                {:oauth-callback {:request-token request-token
                                                  :return-to (get-in context [:request :query-params :return-to])}}}})))}))

(defn not-allowed [context]
  (impl-interceptor/terminate
   (combine context
            {:response {:status 401
                        :body "not allowed"}})))

(defn callback [find-or-create-by-twitter-id]
  (interceptor/interceptor
   {:leave (fn [context]
             (let [{oauth-token :oauth_token
                    oauth-verifier :oauth_verifier} (get-in context [:request :query-params])
                    {:keys [return-to request-token]} (get-in context [:request :session :oauth-callback])
                    consumer (get-consumer)]
                       (if (and oauth-token oauth-verifier return-to request-token)
                         ;; access-token makes an http request to twitter
                         (if-let [user-info (oauth/access-token consumer request-token oauth-verifier)]
                           ;; user-info {:user_id "3295118410",
                           ;;            :screen_name "thecljvector"}
                           (let [twitter-id (get-in user-info [:user_id])
                                 twitter-screen-name (get-in user-info [:screen_name])
                                 {:keys [id name] :as user} (find-or-create-by-twitter-id context twitter-id)]
                             (if name
                               (combine context
                                        {:response
                                         {:status 303
                                          :headers {"Location" return-to}
                                          :session {:auth {:id id}}
                                          :flash {:message "Logged in through Twitter"}
                                          }})
                               (combine context
                                        {:response
                                         {:status 303
                                          :headers {"Location" (link/link context :oauth-create-name :params {:return-to return-to
                                                                                                              :suggested-name twitter-screen-name})}
                                          :session {:auth-create-name {:id id}}}})))
                           (not-allowed context))
                         (not-allowed context))))}))
