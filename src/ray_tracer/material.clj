(ns ray-tracer.material
  (:require [ray-tracer.utils :as utils])
  (:require [ray-tracer.ray :as ray])
  (:require [ray-tracer.vec3 :as vec3]))

(defprotocol Material
  (scatter [mat ray-in record]))

(defrecord Lambertian [albedo]
  Material
  (scatter [mat ray-in record]
    (let [scatter-direction (vec3/+ (:normal record) (utils/rand-unit-vec3))]
    (if (vec3/near-zero scatter-direction)
      [[(:p record) (:normal record)] albedo]
      [[(:p record) scatter-direction] albedo]))))

(defrecord Metal [albedo fuzz]
  Material
  (scatter [mat ray-in record]
    (let [reflected (vec3/reflect (vec3/unit-vector (ray/direction ray-in))
                                  (:normal record))
          scattered [(:p record) (vec3/+ reflected (vec3/* (utils/rand-unit-vec3) fuzz))]]
      (when (pos? (vec3/dot (ray/direction scattered) (:normal record)))
        [scattered albedo]))))

(defn reflectance [cosine eta]
  (let [r0 (/ (- 1 eta) (+ 1 eta))
        r0sq (* r0 r0)]
    (+ r0sq (* (- 1 r0sq) (Math/pow (- 1 cosine) 5)))))

(defrecord Dielectric [eta]
  Material
  (scatter [mat ray-in record]
    (let [eta-ratio (if (:front-face record) (/ 1.0 eta) eta)
          unit-direction (vec3/unit-vector (ray/direction ray-in))
          cos-theta (min (- (vec3/dot unit-direction (:normal record))) 1)
          sin-theta (Math/sqrt (- 1 (* cos-theta cos-theta)))
          cannot-refract (> (* eta-ratio sin-theta) 1)]
      (if (or cannot-refract (> (reflectance cos-theta eta) (rand)))
        (let [reflected (vec3/reflect unit-direction (:normal record))]
          [[(:p record) reflected] [1 1 1]])
        (let [refracted (vec3/refract unit-direction (:normal record) eta-ratio)]
          [[(:p record) refracted] [1 1 1]])))))



(defn scattered [result] (first result))
(defn attenuation [result] (second result))