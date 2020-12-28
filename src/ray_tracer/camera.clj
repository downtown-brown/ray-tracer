(ns ray-tracer.camera
  (:require [ray-tracer.vec3 :as vec3]))

(def aspect-ratio (/ 16 9))
(def viewport-h 2.0)
(def viewport-w (* viewport-h aspect-ratio))

(def origin [0 0 0])
(def horizontal [viewport-w 0 0])
(def vertical [0 viewport-h 0])
(def focal_length 1.0)

;; TODO rewrite plus and minus function to fix this (multiple arguments)

(def lower_left (vec3/-
                 (vec3/-
                  (vec3/-
                   origin
                   (vec3// horizontal 2))
                  (vec3// vertical 2))
                 [0 0 focal_length]))

(defn get-ray [u v]
  [origin (vec3/+
           (vec3/+
            (vec3/- lower_left origin)
            (vec3/* horizontal u))
           (vec3/* vertical v))])