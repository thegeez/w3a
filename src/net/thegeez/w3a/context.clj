(ns net.thegeez.w3a.context
  (:require [io.pedestal.impl.interceptor :as impl-interceptor]))

;; from liberator:
;; A more sophisticated update of the request than a simple merge
;; provides.  This allows decisions to return maps which modify the
;; original request in the way most probably intended rather than the
;; over-destructive default merge.
(defn combine
  "Merge two values such that two maps a merged, two lists, two
  vectors and two sets are concatenated.
  Maps will be merged with maps. The map values will be merged
  recursively with this function.
  Lists, Vectors and Sets will be concatenated with values that are
  `coll?` and will preserve their type.
  For other combination of types the new value will be returned.
  If the newval has the metadata attribute `:replace` then it will
  replace the value regardless of the type."
  [curr newval]
  (cond
   (-> newval meta :replace) newval
   (and (map? curr) (map? newval)) (merge-with combine curr newval)
   (and (list? curr) (coll? newval)) (concat curr newval)
   (and (vector? curr) (coll? newval)) (into curr newval)
   (and (set? curr) (coll? newval)) (set (concat curr newval))
   :otherwise newval))


(defn terminate
  ([context]
     (impl-interceptor/terminate context))
  ([context status]
     (impl-interceptor/terminate
      (combine context
               {:response {:status status}}))))

;; TODO add (flash ..) (status ..) (data ..) etc
