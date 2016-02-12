(ns net.thegeez.w3a.components.server
  (:require [io.pedestal.log :as log]
            [io.pedestal.http.cors :as cors]
            [io.pedestal.http :as http]
            [io.pedestal.http.route :as route]
            [io.pedestal.http.body-params :as body-params]
            [io.pedestal.http.csrf :as csrf]
            [io.pedestal.http.ring-middlewares :as middlewares]
            [io.pedestal.http.secure-headers :as secure-headers]
            [io.pedestal.interceptor :as interceptor]
            [com.stuartsierra.component :as component]
            [net.thegeez.w3a.service-interceptors :as service-interceptors]))

(defn add-component-interceptors [service-map component]
  (update-in service-map [::http/interceptors]
             into (for [[k path] (::component->context service-map)]
                    (when-let [part (get-in component path)]
                      (interceptor/interceptor
                       {:enter (fn [context]
                                 (assoc context k part))})))))

;; ring puts :form-params also in :params, do this for :edn-params as well
;; https://github.com/ring-clojure/ring/blob/de6ea40c6a56e88dbfd485fbbc3f9cf2c9c1ebb1/ring-core/src/ring/middleware/params.clj#L21
(def merge-edn-params-interceptor
  (interceptor/interceptor
   {:name ::merge-params-interceptor
    :enter (fn [context]
             (update-in context [:request :params]
                        merge (get-in context [:request :edn-params])))}))



(defn add-default-interceptors [service-map session-options]
  ;; TODO copy all the config options and defaults from pedestal
  (let [{:keys [::http/interceptors
                ::http/default-interceptors
                ::http/allowed-origins
                ::http/resource-path
                ::http/router
                ::http/routes]} service-map]
    (assoc service-map ::http/interceptors
           (cond-> (or interceptors [])
                   (not (nil? allowed-origins))
                   (conj (cors/allow-origin allowed-origins))
                   true
                   (into [http/not-found
                          (middlewares/session session-options)
                          (body-params/body-params)
                          merge-edn-params-interceptor
                          (middlewares/nested-params)
                          middlewares/keyword-params
                          route/query-params
                          (route/method-param [:form-params "_method"])
                          (csrf/anti-forgery)
                          (middlewares/content-type)
                          service-interceptors/w3a-session-interceptor
                          service-interceptors/w3a-flash-interceptor
                          service-interceptors/w3a-html-flow-interceptor])
                   (not (nil? resource-path))
                   (conj (middlewares/resource resource-path))
                   default-interceptors
                   (into default-interceptors)
                   true
                   (into [(secure-headers/secure-headers {})
                          (route/router routes router)])
                   ))))

(defrecord PedestalComponent [service database session-options]
  component/Lifecycle
  (start [component]
    (log/info :msg "Starting Pedestal component" :service-env (:env service))
    (let [server (-> service
                     (add-component-interceptors component)
                     ;; (add-sessions session-options)
                     
                     ;; ((fn [service]
                     ;;    (let [interceptors (::http/interceptors service)]
                     ;;      (-> service
                     ;;          (dissoc ::http/interceptors)
                     ;;          ;; this is a noop when
                     ;;          ;; ::http/interceptors set
                     ;;          http/default-interceptors
                     ;;          (update-in [::http/interceptors]
                     ;;                     (partial into interceptors))))))
                     (add-default-interceptors session-options)
                     http/create-server)]
      (assoc component :server server)))
  (stop [component]
    (log/info :msg "Stopping Pedestal production component")
    (dissoc component :server)))

(defn pedestal-component [service]
  (map->PedestalComponent {:service service}))

(defrecord JettyComponent [server]
  component/Lifecycle
  (start [component]
    (log/info :msg "Starting jetty component")
    (let [server (-> (:server server)
                     http/start)]
      (assoc component :server server)))
  (stop [component]
    (log/info :msg "Stopping jetty component")
    (when-let [server (:server component)]
      (http/stop (:server server)))
    (dissoc component :server)))

(defn jetty-component []
  (map->JettyComponent {}))
