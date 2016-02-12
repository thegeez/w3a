(ns net.thegeez.w3a.components.sql-database
  (:require [io.pedestal.log :as log]
            [com.stuartsierra.component :as component]
            [clojure.java.jdbc :as jdbc])
  (:import [java.net URI]
           [com.mchange.v2.c3p0 ComboPooledDataSource]))

(defrecord SQLDatabase [db-connect-string]
  component/Lifecycle
  (start [component]
    (log/info :msg "Starting sql database")
    (let [cpds (doto (ComboPooledDataSource.)
                 (.setJdbcUrl db-connect-string))
          spec {:datasource cpds
                :connection-uri db-connect-string}]
      (try (jdbc/query spec ["VALUES 1"]) ;; derbydb
           (catch Exception e
             (try (jdbc/query spec ["SELECT NOW()"]) ;; postgres
                  (catch Exception e
                    (log/info :msg "DB connection failed:" :e e :stack-trace (with-out-str (.printStackTrace e)))))))
      (assoc component :spec spec)))

  (stop [component]
    (log/info :msg "Stopping sql database")
    (.close (:datasource (:spec component)))
    component))

(defn sql-database [db-connect-string]
  (map->SQLDatabase {:db-connect-string db-connect-string}))
