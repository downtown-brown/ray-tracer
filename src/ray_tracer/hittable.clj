(ns ray-tracer.hittable
  (:require [ray-tracer.ray :as ray])
  (:require [ray-tracer.utils :as utils])
  (:require [ray-tracer.vec3 :as vec3]))

(defn hit-record [ray t center radius material]
  (let [p (ray/at ray t)
        normal (vec3// (vec3/- p center) radius)
        front-face (neg? (vec3/dot (ray/direction ray) normal))
        outward-normal (if front-face normal (vec3/- [0 0 0] normal))]
    {:t t :p p :normal outward-normal :material material :front-face front-face}))

(defprotocol Hittable
  (hit [obj ray t-min t-max]))

(defrecord Sphere [center radius material]
  Hittable
  (hit [obj ray t-min t-max]
    (let [dir (ray/direction ray)
          org (ray/origin ray)
          oc (vec3/- org center)
          a (vec3/length-squared dir)
          half-b (vec3/dot oc dir)
          c (- (vec3/length-squared oc) (* radius radius))
          discr (- (* half-b half-b) (* a c))]
      (when (pos? discr)
        (let [root (/ (- (- half-b) (Math/sqrt discr)) a)]
          (if (utils/in-range? root t-min t-max)
            (hit-record ray root center radius material)
            (let [root (/ (+ (- half-b) (Math/sqrt discr)) a)]
              (when (utils/in-range? root t-min t-max)
                (hit-record ray root center radius material)))))))))


(defn check-world [world ray t-min t-max]
  (let [closest-so-far (atom t-max)
        record (atom nil)]
    (doseq [obj world]
      (when-let [rec (hit obj ray t-min @closest-so-far)]
        (reset! closest-so-far (:t rec))
        (reset! record rec)))
    @record))
