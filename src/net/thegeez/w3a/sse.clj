(ns net.thegeez.w3a.sse
  (:require [clojure.core.async :as async]
            [io.pedestal.log :as log]
            [io.pedestal.http.sse :as sse]))

;; dirty hack as Pedestal SSE doesn't do :id field on SSE, so we use
;; :name instead
(alter-var-root #'io.pedestal.http.sse/EVENT_FIELD (fn [_]
                                                     (.getBytes "id: " "UTF-8")))

(defn get-start-from [context]
  (or (try (inc (Long/parseLong (get-in context [:request :headers "last-event-id"])))
           (catch Exception e nil))
      ;; the polyfill fix for IE8/9
      (try (inc (Long/parseLong (get-in context [:request :query-params :evs_last_event_id])))
           (catch Exception e nil))
      (try (inc (Long/parseLong (get-in context [:request :query-params :start_from])))
           (catch Exception e nil))
      0))

(defn stream-start-from
  ([sample-events]
     (stream-start-from sample-events {}))
  ([sample-events opts]
     ;; sample-events :: context start-from -> [{:id ..} ....]
     (let [stream-ready-fn (fn [event-chan context]
                             (let [start-from (get-start-from context)]
                               (async/go
                                 (loop [i 0
                                        start-from start-from]
                                   (let [events-since (sample-events context start-from)
                                         start-from (loop [[event & events] events-since
                                                           start-from start-from]
                                                      (if event
                                                        (let [id (:id event)]
                                                          (if (async/>! event-chan { ;; :name
                                                                                    ;; is used
                                                                                    ;; for :id
                                                                                    :name id
                                                                                    :data (pr-str event)})
                                                            (recur events (inc id))
                                                            start-from))
                                                        start-from))]
                                     (async/<! (async/timeout 1000))
                                     (when (< i 20)
                                       (recur (inc i) start-from))))
                                 (async/close! event-chan))))]
       (sse/start-event-stream stream-ready-fn
                               10 ;; default pedestal heartbeat-delay
                               10 ;; default pedestal bufferfn-or-n


                               ;; TODO pedestal version conflict while
                               ;; adding drawbridge to gatherlist
                               ;; hacky disabled for now
                              ;; opts ;; may contain :on-client-disconnect (fn [context])
                               ))))
