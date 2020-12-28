(ns ray-tracer.utils
  (:require [ray-tracer.vec3 :as vec3]))

(defn rand-in-range [lo hi]
  (+ (rand (- hi lo)) lo))

(defn rand-unit-vec3 []
  (let [vec [(rand-in-range -1 1)
             (rand-in-range -1 1)
             (rand-in-range -1 1)]]
    (if (>= (vec3/length-squared vec) 1)
      (rand-unit-vec3)
      (vec3/unit-vector vec))))

(defn in-range? [t t-min t-max]
  (and (> t t-min) (< t t-max)))

(defn clamp [x min max]
  (if (< x min)
    min
    (if (> x max)
      max
      x)))