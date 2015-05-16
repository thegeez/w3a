(ns net.thegeez.w3a.server
  (:require [io.pedestal.log :as log]
            [io.pedestal.http :as http]
            [io.pedestal.http.body-params :as body-params]
            [io.pedestal.http.csrf :as csrf]
            [io.pedestal.http.ring-middlewares :as middlewares]
            [io.pedestal.interceptor :as interceptor]
            [com.stuartsierra.component :as component]))

(defn add-database-interceptor [database]
  (interceptor/interceptor
   {:name ::add-database-interceptor
    :enter (fn [context]
             (assoc context :database (:connection database)))}))

(defn add-database [service-map database]
  (update-in service-map [::http/interceptors]
             conj (add-database-interceptor database)))

(defn add-sessions [service-map session-options]
  (update-in service-map [::http/interceptors]
             conj (middlewares/session session-options)))

;; ring puts :form-params also in :params, do this for :edn-params as well
;; https://github.com/ring-clojure/ring/blob/de6ea40c6a56e88dbfd485fbbc3f9cf2c9c1ebb1/ring-core/src/ring/middleware/params.clj#L21
(def merge-edn-params-interceptor
  (interceptor/interceptor
   {:name ::merge-params-interceptor
    :enter (fn [context]
             (update-in context [:request :params]
                        merge (get-in context [:request :edn-params])))}))

(defn add-default-interceptors [service-map]
  (cond-> service-map
          (::http/default-interceptors service-map)
          (update-in [::http/interceptors] into (::http/default-interceptors service-map))
          true
          (update-in [::http/interceptors]
                     into [(body-params/body-params)
                           merge-edn-params-interceptor
                           (middlewares/nested-params)
                           middlewares/keyword-params
                           (csrf/anti-forgery)
                           (middlewares/flash)])))

(defrecord PedestalComponent [service database session-options]
  component/Lifecycle
  (start [component]
    (log/info :msg "Starting Pedestal component" :service-env (:env service))
    (let [server (-> service
                     (add-database database)
                     (add-sessions session-options)
                     add-default-interceptors
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
