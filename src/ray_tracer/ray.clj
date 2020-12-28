(ns ray-tracer.ray
  (:require [ray-tracer.vec3 :as vec3]))

(defn origin [ray]
  (first ray))

(defn direction [ray]
  (second ray))

(defn at [ray t]
  (vec3/+ (origin ray)
          (vec3/* (direction ray) t)))
