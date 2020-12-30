(ns ray-tracer.camera
  (:require [ray-tracer.vec3 :as vec3]))


(defn make-camera [vfov aspect-ratio lookfrom lookat vup]
  (let [theta (Math/toRadians vfov)
        h (Math/tan (/ theta 2))
        viewport-h (* 2 h)
        viewport-w (* aspect-ratio viewport-h)
        w (vec3/unit-vector (vec3/- lookfrom lookat))
        u (vec3/unit-vector (vec3/cross vup w))
        v (vec3/cross w u)
        horizontal (vec3/* u viewport-w)
        vertical (vec3/* v viewport-h)
        lower-left (vec3/-
                    (vec3/-
                     (vec3/-
                      lookfrom
                      (vec3// horizontal 2))
                     (vec3// vertical 2))
                    w)]
    (fn [s t]
      [lookfrom (vec3/+
                 (vec3/+
                  (vec3/- lower-left lookfrom)
                  (vec3/* horizontal s))
                 (vec3/* vertical t))])))