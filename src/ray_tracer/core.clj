(ns ray-tracer.core
  (:gen-class)
  (:require [clojure.java.io :as io])
  (:require [ray-tracer.ray :as ray])
  (:require [ray-tracer.hittable :as hittable])
  (:require [ray-tracer.vec3 :as vec3]))

(def ^:const width 400)
(def ^:const height 225)

(def ^:const viewport_h 2.0)
(def ^:const viewport_w 3.5555555)

(def ^:const origin [0 0 0])
(def ^:const horizontal [viewport_w 0 0])
(def ^:const vertical [0 viewport_h 0])
(def ^:const focal_length 1.0)
(def lower_left (vec3/- (vec3/- (vec3/- origin
                        (vec3// horizontal 2))
                        (vec3// vertical 2))
                        [0 0 focal_length]))

(def world [(hittable/map->Sphere {:center [0 0 -1] :radius 0.5})
            (hittable/map->Sphere {:center [0 -100.5 -1] :radius 100})])

(defn write-color [[r g b] out-file]
  (let [ir (int (* r 255.999))
        ig (int (* g 255.999))
        ib (int (* b 255.999))]
  (.write out-file (str ir " " ig " " ib "\n"))))

(defn ray-color [ray world]
  (let [record (hittable/check-world world ray 0 ##Inf)]
    (if record
      (vec3/* (vec3/+ (:normal record) [1 1 1]) 0.5)
      (let [unit-direction (vec3/unit-vector (ray/direction ray))
            tmp (* (+ 1 (vec3/y unit-direction)) 0.5)]
        (vec3/+ (vec3/* [1.0 1.0 1.0] (- 1.0 tmp))
                (vec3/* [0.5 0.7 1.0] tmp))))))

(defn -main
  "Writes out a test image"
  []
  (with-open [out-file (io/writer "test.ppm")]
    (.write out-file (str "P3\n" width " " height "\n255\n"))
    (doseq [j (range (- height 1) -1 -1)]
      (print "\rScanlines remaining: " j " ")
      (flush)
      (doseq [i (range 0 width)]
        (let [u (/ i (- width 1))
              v (/ j (- height 1))
              pixel-color (ray-color [origin (vec3/+ (vec3/+ (vec3/- lower_left origin)
                                                             (vec3/* horizontal u))
                                                     (vec3/* vertical v))]
                                     world)]
        (write-color pixel-color out-file)))))
  (print "\nDone\n"))



