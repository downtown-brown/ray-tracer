(ns ray-tracer.sphere  
  (:require [ray-tracer.ray :as ray])
  (:require [ray-tracer.vec3 :as vec3]))

(defn hit [center radius ray]
  (let [dir (ray/direction ray)
        org (ray/origin ray)
        oc (vec3/- org center)
        a (vec3/length-squared dir)
        half-b (vec3/dot oc dir)
        c (- (vec3/length-squared oc) (* radius radius))
        discr (- (* half-b half-b) (* a c))]
    (if (neg? discr)
      -1.0
      (/ (- (- half-b) (Math/sqrt discr)) a))))
