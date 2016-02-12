(ns net.thegeez.w3a.html
  (:require [clojure.string :as string]
            [clojure.walk :as walk]
            [fipp.edn :as fipp]
            [hiccup.util :as hiccup-util]
            [io.pedestal.interceptor :as interceptor]
            [net.thegeez.w3a.context :refer [combine]]))

(defn for-html
  "status is a http status code
template-fn: context -> string | seq of strings (from enlive)
noop when request is not for html"
  [status template-fn]
  (interceptor/interceptor
   {:leave (fn [context]
             (if (and (.contains ^String (get-in context [:request :headers "accept"] "") "text/html")
                      (not= "text/html" (get-in context [:response :headers "Content-Type"])))
               (cond-> context
                (= (get-in context [:response :status]) status)
                (combine {:response {:headers {"Content-Type" "text/html"}
                                     :body (let [body (template-fn context)]
                                             (cond
                                              (string? body)
                                              body
                                              (seq body) ;; for enlive
                                              (apply str body)
                                              :else body))}}))
               context))}))

(defn edn->html [edn]
  (let [pprint-str (with-out-str (fipp/pprint edn))
        body (->> (hiccup-util/escape-html pprint-str)
                  (string/split-lines)
                  (string/join "<br/>"))
        link-replacements (let [collect-links (atom {})]
                            (walk/prewalk
                             (fn [elem]
                               (when (and (string? elem)
                                          (or (.startsWith ^String elem "http")
                                              (.startsWith ^String elem "/")))
                                 (swap! collect-links assoc (str "&quot;" elem "&quot;") (str "<a href=\"" elem "\">" elem "</a>")))
                               elem)
                             edn)
                            @collect-links)
        body (reduce-kv
              string/replace
              body
              link-replacements)]
    (str "<pre>" body "</pre>")))

(defn basic-interceptor
  ([] (basic-interceptor "PLACEHOLDER"))
  ([fn-or-body]
     (interceptor/interceptor
      {:enter (fn [context]
                (combine context
                         {:response {:status 200
                                     :body (if (fn? fn-or-body)
                                             (fn-or-body context)
                                             fn-or-body)}}))})))
