(ns ray-tracer.camera
  (:require [ray-tracer.vec3 :as vec3])
  (:require [ray-tracer.utils :as utils]))

(defrecord Camera [lookfrom
                   lookat
                   vup
                   vfov
                   aspect-ratio
                   aperture
                   focus-dist])

(defn make-camera [^Camera cam]
  (let [theta (Math/toRadians (:vfov cam))
        h (Math/tan (/ theta 2))
        viewport-h (* 2 h)
        viewport-w (* (:aspect-ratio cam) viewport-h)
        w (vec3/unit-vector (vec3/- (:lookfrom cam) (:lookat cam)))
        u (vec3/unit-vector (vec3/cross (:vup  cam)w))
        v (vec3/cross w u)
        horizontal (vec3/* (vec3/* u viewport-w) (:focus-dist cam))
        vertical (vec3/* (vec3/* v viewport-h) (:focus-dist cam))
        lower-left (vec3/-
                    (vec3/-
                     (vec3/-
                      (:lookfrom cam)
                      (vec3// horizontal 2))
                     (vec3// vertical 2))
                    (vec3/* w (:focus-dist cam)))
        lens-radius (/ (:aperture cam) 2)]
    (fn [s t]
      (let [rd (vec3/* (utils/rand-unit-vec3) lens-radius)
            offset (vec3/+ (vec3/* u (vec3/x rd))
                           (vec3/* v (vec3/y rd)))]
      [(vec3/+ (:lookfrom cam) offset)
       (reduce vec3/+ [(reduce vec3/- [lower-left offset (:lookfrom cam)])
               (vec3/* horizontal s)
               (vec3/* vertical t)])]))))