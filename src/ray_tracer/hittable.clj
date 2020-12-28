(ns ray-tracer.hittable
  (:require [ray-tracer.ray :as ray])
  (:require [ray-tracer.utils :as utils])
  (:require [ray-tracer.vec3 :as vec3]))

(defn hit-record [ray t center radius]
  (let [p (ray/at ray t)
        normal (vec3// (vec3/- p center) radius)
        front-face (neg? (vec3/dot (ray/direction ray) normal))
        outward-normal (if front-face normal (vec3/- [0 0 0] normal))]
    {:t t :p p :normal outward-normal}))

(defprotocol Hittable
  (hit [this ray t-min t-max]))

(defrecord Sphere [center radius]
  Hittable
  (hit [this ray t-min t-max]
    (let [dir (ray/direction ray)
          org (ray/origin ray)
          oc (vec3/- org (:center this))
          a (vec3/length-squared dir)
          half-b (vec3/dot oc dir)
          r (:radius this)
          c (- (vec3/length-squared oc) (* r r))
          discr (- (* half-b half-b) (* a c))]
      (when (pos? discr)
        (let [root (/ (- (- half-b) (Math/sqrt discr)) a)]
          (if (utils/in-range? root t-min t-max)
            (hit-record ray root (:center this) r)
            (let [root (/ (+ (- half-b) (Math/sqrt discr)) a)]
              (when (utils/in-range? root t-min t-max)
                (hit-record ray root (:center this) r)))))))))


(defn check-world [world ray t-min t-max]
  (let [closest-so-far (atom t-max)
        record (atom nil)]
    (doseq [obj world]
      (when-let [rec (hit obj ray t-min @closest-so-far)]
        (reset! closest-so-far (:t rec))
        (reset! record rec)))
    @record))