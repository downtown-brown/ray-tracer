(ns ray-tracer.core
  (:gen-class)
  (:require [clojure.java.io :as io])
  (:require [ray-tracer.ray :as ray])
  (:require [ray-tracer.hittable :as hittable])
  (:require [ray-tracer.camera :as camera])
  (:require [ray-tracer.utils :as utils])
  (:require [ray-tracer.vec3 :as vec3]))

(def ^:const width 400)
(def ^:const height 225)
(def ^:const samples-per-pixel 10)


(def world [(hittable/map->Sphere {:center [0 0 -1] :radius 0.5})
            (hittable/map->Sphere {:center [0 -100.5 -1] :radius 100})])

(defn write-color [[r g b] out-file]
  (let [ir (int (* (utils/clamp r 0 0.999) 256))
        ig (int (* (utils/clamp g 0 0.999) 256))
        ib (int (* (utils/clamp b 0 0.999) 256))]
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
        (let [samples (map (fn [_] (let [u (/ (+ i (rand)) (- width 1))
                                        v (/ (+ j (rand)) (- height 1))]
                                    (vec3// (ray-color (camera/get-ray u v) world)
                                            samples-per-pixel))) (range 0 samples-per-pixel))
              pixel-color (reduce vec3/+ samples)]
          (write-color pixel-color out-file)))))
  (print "\nDone\n"))



