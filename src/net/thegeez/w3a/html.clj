(ns net.thegeez.w3a.html
  (:require [io.pedestal.log :as log]
            [net.cgrand.enlive-html :as html]))

(defn flash-html [context]
  (log/info :flash-html-context (get-in context [:response :data :flash])
            :request-flash (get-in context [:request :flash]))
  (if-let [msg (get-in context [:response :data :flash :message])]
    (html/before
     (html/html [:div {:id "flash"
                       :class "alert alert-info"} msg]))
    identity))

(defmacro html-string [nodes & rules]
  `(apply str
          (html/emit*
           (html/at ~nodes
                    ~@(into [[:#content]
                             `(flash-html ~'context)]
                            rules)))))
