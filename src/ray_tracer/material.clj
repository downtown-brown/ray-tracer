(ns ray-tracer.material
  (:require [ray-tracer.utils :as utils])
  (:require [ray-tracer.ray :as ray])
  (:require [ray-tracer.vec3 :as vec3]))

(defprotocol Material
  (scatter [this ray-in record]))

(defrecord Lambertian [albedo]
  Material
  (scatter [this ray-in record]
  (let [scatter-direction (vec3/+ (:normal record) (utils/rand-unit-vec3))]
    (if (vec3/near-zero scatter-direction)
      [[(:p record) (:normal record)] albedo]
      [[(:p record) scatter-direction] albedo]))))

(defrecord Metal [albedo fuzz]
  Material
  (scatter [this ray-in record]
    (let [reflected (vec3/reflect (vec3/unit-vector (ray/direction ray-in))
                                  (:normal record))
          scattered [(:p record) (vec3/+ reflected (vec3/* (utils/rand-unit-vec3) fuzz))]]
      (when (pos? (vec3/dot (ray/direction scattered) (:normal record)))
        [scattered albedo]))))

(defn scattered [result] (first result))
(defn attenuation [result] (second result))