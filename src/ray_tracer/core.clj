(ns ray-tracer.core
  (:gen-class)
  (:require [clojure.java.io :as io])
  (:require [ray-tracer.ray :as ray])
  (:require [ray-tracer.hittable :as hittable])
  (:require [ray-tracer.material :as material])
  (:require [ray-tracer.camera :as camera])
  (:require [ray-tracer.utils :as utils])
  (:require [ray-tracer.vec3 :as vec3]))

(def ^:const width 400)
(def ^:const height 225)
(def ^:const samples-per-pixel 10)
(def ^:const max-depth 50)

(def world [(hittable/map->Sphere
             {:center [-1 0 -1]
              :radius 0.5
              :material (material/->Metal [0.7 0.2 0.1] 0)})
            (hittable/map->Sphere
             {:center [1 0 -1]
              :radius 0.5
              :material (material/->Metal [0.8 0.1 0.9] 0)})
            (hittable/map->Sphere
             {:center [0 0 -1]
              :radius 0.5
              :material (material/->Lambertian [0.1 0.2 0.5])})
            (hittable/map->Sphere
             {:center [0 -100.5 -1]
              :radius 100
              :material (material/->Lambertian [0.8 0.8 0.0])})])

(defn write-color [[r g b] out-file]
  (let [ir (int (* (utils/clamp (Math/sqrt r) 0 0.999) 256))
        ig (int (* (utils/clamp (Math/sqrt g) 0 0.999) 256))
        ib (int (* (utils/clamp (Math/sqrt b) 0 0.999) 256))]
  (.write out-file (str ir " " ig " " ib "\n"))))

(defn ray-color [ray world depth]
  (if (pos? depth)
    (let [record (hittable/check-world world ray 0.001 ##Inf)]
      (if record
        (let [result (material/scatter (:material record) ray record)
              scattered (material/scattered result)
              attenuation (material/attenuation result)]
          (if result
            (vec3/* attenuation (ray-color scattered world (- depth 1)))
            [0 0 0]))
        (let [unit-direction (vec3/unit-vector (ray/direction ray))
              tmp (* (+ 1 (vec3/y unit-direction)) 0.5)]
          (vec3/+ (vec3/* [1.0 1.0 1.0] (- 1.0 tmp))
                  (vec3/* [0.5 0.7 1.0] tmp)))))
  [0 0 0]))

(defn do-samples [i j world _]
  (let [u (/ (+ i (rand)) (- width 1))
        v (/ (+ j (rand)) (- height 1))]
    (vec3// (ray-color (camera/get-ray u v) world max-depth)
            samples-per-pixel)))

(defn -main
  "Writes out a test image"
  []
  (with-open [out-file (io/writer "test.ppm")]
    (.write out-file (str "P3\n" width " " height "\n255\n"))
    (doseq [j (range (- height 1) -1 -1)]
      (print "\rScanlines remaining: " j " ")
      (flush)
      (doseq [i (range 0 width)]
        (let [samples (pmap #(do-samples i j world %) (range 0 samples-per-pixel))
              pixel-color (reduce vec3/+ samples)]
          (write-color pixel-color out-file)))))
  (print "\nDone\n"))

