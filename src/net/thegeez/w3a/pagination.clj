(ns net.thegeez.w3a.pagination
  (:require [io.pedestal.log :as log]
            [net.cgrand.enlive-html :as html]
            [net.thegeez.w3a.link :as link]))

(defn pagination-params [context]
  (let [page (try (Long/parseLong (get-in context [:request :params :page]))
                  (catch Exception _
                    1))
        limit (try (Long/parseLong (get-in context [:request :params :limit]))
                   (catch Exception _
                     10))]
    {:page page
     :limit limit}))

(defn links [context kw-name pagination count]
  (let [{:keys [page limit]} pagination
        max-page (inc (long (Math/ceil (/ count limit))))
        next-page (when (< (* page limit) count)
                    (inc page))
        prev-page (when (<= 1 (dec page))
                    (dec page))]
    {:next (when next-page
             (link/link context kw-name :params {:page next-page}))
     :previous (when prev-page
                 (link/link context kw-name :params {:page prev-page}))
     :pages (for [i (range 1 max-page)]
              {:index i
               :href (link/link context kw-name :params {:page i})
               :active (= i page)})}))

(defn html [pagination]
  (html/html [:ul.pagination
              (if-let [prev (:previous pagination)]
                [:li
                 [:a {:href prev} "«"]]
                [:li.disabled
                 [:a {:href ""} "«"]])
              (for [{:keys [index href active]} (:pages pagination)]
                [:li (when active
                       {:class "active"})
                 [:a {:href href} (str index)]])
              (if-let [next (:next pagination)]
                [:li
                 [:a {:href next} "»"]]
                [:li.disabled
                 [:a {:href ""} "»"]])]))
