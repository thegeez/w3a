(ns net.thegeez.w3a.test
  (:require [clojure.edn :as edn]
            [com.stuartsierra.component :as component]
            [io.pedestal.http :as http]
            [io.pedestal.interceptor :as interceptor]
            [io.pedestal.log :as log]
            [io.pedestal.test :as test]
            [net.cgrand.enlive-html :as html]
            [kerodon.core :as kerodon]
            [peridot.request]))

;; make ring mock not munge edn
(let [old-fn (get (methods ring.mock.request/body) java.util.Map)]
  (remove-method ring.mock.request/body java.util.Map)
  (defmethod ring.mock.request/body java.util.Map
    [request body]
    (if (-> body meta :edn)
      (ring.mock.request/body request (pr-str body))
      (old-fn request body))))


(def csrf-token-in-response-header
  (interceptor/interceptor
   {:name ::csrf-token-in-response-header
    :leave (fn [context]
             (assoc-in context [:response :headers "X-TEST-HELPER-CSRF"]
                       (or (get-in context [:request :io.pedestal.http.csrf/anti-forgery-token])
                           (get-in context [:request :session "__anti-forgery-token"]))))}))

;; allow to find the csrf-token to use in testing
(defn surface-csrf-token [system]
  (update-in system [:server :service ::http/interceptors]
             conj csrf-token-in-response-header))

(defn system->ring-handler [system]
  (let [system (-> system
                   surface-csrf-token
                   (cond->
                    (.startsWith ^String (get-in system [:db :db-connect-string] "")
                                 "jdbc:derby:memory:")
                    ;; TODO make this an explicit component
                    (update-in [:db :db-connect-string]
                               #(str "jdbc:derby:memory:" (gensym "pertestdb") (subs % (count "jdbc:derby:memory:")))))
                   (dissoc :jetty))
        service-fn (-> system
                       component/start
                       (get-in [:server :server ::http/service-fn]))]
    (fn [req]
      (let [method (:request-method req)
            uri (:uri req)
            ;; pedestal blows up when it can't figure out scheme /
            ;; host etc, which is missing as jetty is not running
            uri (if (not (.startsWith ^String uri "http"))
                  (str "http://testhost" uri)
                  uri)
            uri (if-let [qs (:query-string req)]
                  (str uri "?" qs)
                  uri)
            options (cond-> []
                            (:body req)
                            (into [:body (slurp (:body req))])
                            (:headers req)
                            (into [:headers (zipmap (map #(get {"content-type" "Content-Type"
                                                                "accept" "Accept"} % %) (keys (:headers req)))
                                                    (vals (:headers req)))]))

            response (apply test/response-for service-fn method uri options)]
        (cond-> response
                (.startsWith ^String (get-in response [:headers "Content-Type"] "") "application/edn")
                (assoc :edn (try (edn/read-string (get response :body))
                                 (catch Exception e
                                   (log/info :unreadable (get response :body))
                                   (throw e)))))))))


;; broken in kerodon for text only links when links with nested html
;; on a page
(defn follow [state link]
  (kerodon/follow state (if (string? link)
                          (html/pred #(= (first (:content %)) link))
                          link)))

;; TODO make same shape as kerodon testers
(defn at? [res url]
  (clojure.test/is (= url (str (get-in res [:request :uri])
                               ;; only check the query string as well
                               ;; when specified by user
                               (when (.contains url "?")
                                 (when-let [query-string (get-in res [:request :query-string])]
                                   (str "?" query-string))))))
  res)
