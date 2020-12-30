(ns ray-tracer.core
  (:gen-class)
  (:require [clojure.java.io :as io]
            [ray-tracer.ray :as ray]
            [ray-tracer.hittable :as hittable]
            [ray-tracer.material :as material]
            [ray-tracer.camera :as camera]
            [ray-tracer.utils :as utils]
            [ray-tracer.vec3 :as vec3]))

(def ^:const width 400)
(def ^:const height 225)

(def aspect-ratio (/ width height))
(def ^:const samples-per-pixel 10)
(def ^:const max-depth 50)

(def cam (camera/map->Camera {:lookfrom [13 2 3]
                              :lookat [0 0 -1]
                              :vup [0 1 0]
                              :vfov 20.0
                              :aspect-ratio aspect-ratio
                              :aperture 0.1
                              :focus-dist 10.0}))

(def get-ray (camera/make-camera cam))

(def world-const [(hittable/map->Sphere
                   {:center [0 1 0]
                    :radius 1
                    :material (material/->Dielectric 1.5)})
                  (hittable/map->Sphere
                   {:center [4, 1, 0]
                    :radius 1
                    :material (material/->Metal [0.7, 0.6, 0.5] 0)})
                  (hittable/map->Sphere
                   {:center [-4, 1, 0]
                    :radius 1
                    :material (material/->Lambertian [0.4, 0.2, 0.1])})
                  (hittable/map->Sphere
                   {:center [0 -1000 0]
                    :radius 1000
                    :material (material/->Lambertian [0.5, 0.5, 0.5])})])

(defn gen-random-spheres [a b]
  (let [choose-mat (rand)
        center [(+ a (* 0.9 (rand))) 0.2 (+ b (* 0.9 (rand)))]]
    (when (> (vec3/length (vec3/- center [4 0.2 0])) 0.9)
      (cond
        (< choose-mat 0.8)
        (hittable/map->Sphere
         {:center center
          :radius 0.2
          :material (material/->Lambertian
                     (vec3/* (utils/rand-unit-vec3)
                             (utils/rand-unit-vec3)))})
        (< choose-mat 0.95)
        (hittable/map->Sphere
         {:center center
          :radius 0.2
          :material (material/->Metal
                     (vec3/* (utils/rand-unit-vec3) 0.5)
                     (rand 0.5))})
        :else
        (hittable/map->Sphere
         {:center center
          :radius 0.2
          :material (material/->Dielectric 1.5)})))))

(def world-rand
  (vec (remove
        nil? (for [a (range -11 11)
                   b (range -11 11)]
               (gen-random-spheres a b)))))

(def world (vec (concat world-rand world-const)))

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
    (vec3// (ray-color (get-ray u v) world max-depth)
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
        (let [samples (map #(do-samples i j world %) (range 0 samples-per-pixel))
              pixel-color (reduce vec3/+ samples)]
          (write-color pixel-color out-file)))))
  (print "\nDone\n"))

