(ns net.thegeez.w3a.service-interceptors
  (:require [io.pedestal.log :as log]
            [io.pedestal.interceptor :as interceptor]))

(def w3a-session-interceptor
  (interceptor/interceptor
   {:name :w3a-session-interceptor
    :leave
    (fn [context]
      (update-in context [:response :session]
                 (fn [new-session]
                   (let [old-session (-> (get-in context [:request :session])
                                         (cond->
                                          ;; getting a sse response, doesn't count for seeing the flash message
                                          (not= (get-in context [:response :headers "Content-Type"]) "text/event-stream")
                                          (dissoc :_flash)))]
                     (reduce-kv
                      (fn [s k v]
                        (if (contains? s k)
                          (if v
                            (assoc s k v)
                            (dissoc s k))
                          (assoc s k v)))
                      old-session
                      new-session)))))}))

(def w3a-flash-interceptor
  (interceptor/interceptor
   {:name :w3a-flash-interceptor
    :enter
    (fn [context]
      (if-let [flash (get-in context [:request :session :_flash])]
        (assoc-in context [:request :flash] flash)
        context))
    :leave
    (fn [context]
      (if-let [flash (and (get-in context [:response :headers "Location"])
                          (get-in context [:response :flash]))]
        (assoc-in context [:response :session :_flash] flash)
        context))}))

(def w3a-html-flow-interceptor
  (interceptor/interceptor
   {:name :w3a-html-flow-interceptor
    :leave
    (fn [context]
      (if (.contains ^String (get-in context [:request :headers "accept"] "") "text/html")
        (let [status (get-in context [:response :status])]
          (cond
           (and (or (= status 201)
                    (= status 204)
                    (= status 401)
                    (= status 403))
                (contains? (get-in context [:response :headers]) "Location"))
           (assoc-in context [:response :status] 303)
           (= status 422)
           (assoc-in context [:response :status] 200)
           :else
           context))
        context))}))
