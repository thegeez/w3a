(defproject net.thegeez/w3a "0.0.4"
  :url "http://thegeez.net"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.6.0"]
                 [io.pedestal/pedestal.service "0.4.0"]

                 [io.pedestal/pedestal.jetty "0.4.0"]

                 [ch.qos.logback/logback-classic "1.1.2" :exclusions [org.slf4j/slf4j-api]]
                 [org.slf4j/jul-to-slf4j "1.7.7"]
                 [org.slf4j/jcl-over-slf4j "1.7.7"]
                 [org.slf4j/log4j-over-slf4j "1.7.7"]

                 [com.stuartsierra/component "0.2.1"]

                 [org.clojure/java.jdbc "0.3.0"]
                 [postgresql/postgresql "8.4-702.jdbc4"]
                 [com.mchange/c3p0 "0.9.2.1"]

                 [hiccup "1.0.5"]
                 [enlive "1.1.5"]

                 [environ "1.0.1"]

                 [clj-http "2.0.0"]
                 [clj-oauth "1.5.3"]]
    :resource-paths ["config", "resources"]
    :profiles {:dev {:source-paths ["dev"]
                     :dependencies [[ns-tracker "0.2.2"]
                                    [org.clojure/tools.namespace "0.2.3"]
                                    [org.clojure/java.classpath "0.2.0"]
                                    [org.apache.derby/derby "10.8.1.2"]
                                    [kerodon "0.6.0-SNAPSHOT"]
                                    [peridot "0.3.1" :exclusions [clj-time]]]}}

    :repositories [["clojars" {:url "http://clojars.org/repo"
                               :sign-releases false}]])
