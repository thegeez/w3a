(ns net.thegeez.w3a.edn-wrap
  (:require [clojure.edn :as edn]
            [clojure.string :as string]
            [clojure.pprint :as pprint]
            [clojure.walk :as walk]
            [hiccup.core :as hiccup]
            [hiccup.util :as hiccup-util]
            [io.pedestal.log :as log]
            [io.pedestal.interceptor :as interceptor]
            [io.pedestal.impl.interceptor :as impl-interceptor]
            [net.thegeez.w3a.form :as form]
            [net.thegeez.w3a.link :as link]
            [ring.util.response :as ring-response]))

(defmethod pprint/simple-dispatch clojure.lang.Fn
  [obj]
  (pr :function-unprintable))

(defmethod print-method clojure.lang.Fn
  [o w]
  (print-simple :function-unprintable w))

(defn html-edn [context]
  (let [edn (get-in context [:response :data])
        layout-replacements {"\n" "<br/>"}
        body (reduce-kv
              string/replace
              (hiccup-util/escape-html (with-out-str (pprint/pprint edn)))
              layout-replacements)
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

(defn forms-edn [context]
  (let [data (get-in context [:response :data])]
    (when (map? data)
      (for [[name data] data
            :when (and (map? data)
                       (contains? data :net.thegeez.w3a.binding/binding))]
        (form/scaffold-form context name)))))

(defn htmlify-edn [context]
  (assoc-in context [:response :body]
            (let [body (html-edn context)
                  forms (forms-edn context)]
              (str body
                   (string/join (map #(hiccup/html %) forms))))))

(defn for-html? [context]
  (and (.startsWith ^String (get-in context [:request :headers "accept"] "") "text/html")
       (= 200 (get-in context [:response :status]))
       (not= "text/html" (get-in context [:response :headers "Content-Type"]))))

(defn accept-default [context]
  (cond-> context
          (= (get-in context [:request :headers "accept"]) "*/*")
          (assoc-in [:request :headers "accept"] "text/html")))

(defn format-by-param [context]
  (if-let [format-param (get-in context [:request :query-params :format])]
    (let [orig-type (get-in context [:request :headers "accept"])]
      (log/info :fp format-param :context context)
      (-> context
          (assoc-in [:request :headers "accept"]
                    (condp = format-param
                      "edn" "application/edn"
                      "html" "text/html"
                      "dev" "application/edn+html"
                      orig-type))
          (assoc-in [:request :headers "x-original-accept"] orig-type)))
    context))

(def wrap-edn
  (interceptor/interceptor
   {:name ::wrap-edn
    :enter (fn [context]
             (-> context
                 accept-default
                 format-by-param
                 (as-> context
                       (assoc context :self (link/self context)))
                 (cond->
                  (.startsWith ^String (get-in context [:request :content-type] "") "application/edn")
                  (update-in [:request :params] merge (get-in context [:request :edn-params])))))
    :leave (fn [context]
             (let [;; make sessions easier to work with
                   ;; keys are kept when adding other keys, except
                   ;; when the whole session is set to {} or nil
                   context (update-in context [:response :session]
                                      (fn [new-session]
                                        (let [old-session (-> (get-in context [:request :session])
                                                              (dissoc :_flash))]
                                          (if (= {} new-session)
                                            {}
                                            (reduce-kv
                                             (fn [s k v]
                                               (if (contains? s k)
                                                 (if v
                                                   (assoc s k v)
                                                   (dissoc s k))
                                                 (assoc s k v)))
                                             old-session
                                             new-session)))))]
               (cond
                (and (or (.startsWith (get-in context [:request :headers "accept"] "")
                                      "text/html")
                         (.startsWith (get-in context [:request :headers "accept"] "")
                                      "application/edn+html"))
                     (not (.startsWith (get-in context [:response :headers "Content-Type"] "") "text/html")))
                (let [status (get-in context [:response :status])]
                  (cond
                   (= 200 status)
                   (-> context
                       (assoc-in [:response :headers "Content-Type"] "text/html;charset=UTF-8")
                       htmlify-edn)
                   (= 201 status)
                   (-> context
                       (assoc-in [:response :status] 303)
                       (update-in [:response :headers] merge
                                  {"Content-Type" "text/html"}))
                   (= 303 status)
                   (update-in context [:response :headers] merge
                              {"Content-Type" "text/html"})
                   (= 400 status)
                   (let [router (some
                                 (fn [interceptor]
                                   (when (= (:name interceptor) :io.pedestal.http.route/router)
                                     interceptor))
                                 (get-in context [:io.pedestal.impl.interceptor/stack]))]
                     (log/info :sending-back status)
                     (-> context
                         (assoc-in [:request :request-method] :get)
                         (assoc-in [:request :response-state] (get-in context [:response :data]))
                         (assoc-in [:response] {})
                         (impl-interceptor/enqueue router)
                         (impl-interceptor/execute)))

                   :else context))

                (and (.startsWith (get-in context [:request :headers "accept"] "")
                                  "application/edn")
                     (not (.startsWith (get-in context [:response :headers "Content-Type"] "") "application/edn")))
                (let [status (get-in context [:response :status])]
                  (cond
                   (not (get-in context [:response :body]))
                   (-> context
                       (assoc-in [:response :headers "Content-Type"] "application/edn;charset=UTF-8")
                       (assoc-in [:response :body]
                                 (str (pr-str (get-in context [:response :data])) "\n\r")))
                   :else context))

                :else context)))}))
