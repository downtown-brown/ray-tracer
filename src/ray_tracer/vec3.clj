(ns ray-tracer.vec3
  (:require [clojure.core :as clj]))

(defn mute [op
            [^float x1 ^float y1 ^float z1]
            [^float x2 ^float y2 ^float z2]]
  [(op x1 x2) (op y1 y2) (op z1 z2)])

(defn + [v1 v2]
  (if (number? v2)
    (map #(clj/+ v2 %) v1)
    (mute clj/+ v1 v2)))

(defn - [v1 v2]
  (if (number? v2)
    (map #(clj/- v2 %) v1)
    (mute clj/- v1 v2)))

(defn * [v1 v2]
  (if (number? v2)
    (map #(clj/* v2 %) v1)
    (mute clj/* v1 v2)))

(defn / [v1 v2]
  (if (number? v2)
    (if (zero? v2)
      1
      (* v1 (clj// 1 v2)))
    (mute clj// v1 v2)))

(defn dot [v1 v2]
  (reduce clj/+ (* v1 v2)))

(defn cross [[x1 y1 z1] [x2 y2 z3]]
  [(clj/- (clj/* y1 z3) (clj/* z1 y2))
   (clj/- (clj/* z1 x2) (clj/* x1 z3))
   (clj/- (clj/* x1 y2) (clj/* y1 x2))])

(defn length-squared [[x y z]]
  (clj/+ (clj/* x x) (clj/* y y) (clj/* z z)))

(defn length [v]
  (Math/sqrt (length-squared v)))

(defn unit-vector [v]
  (let [l (length v)]
    (map #(clj// % l) v)))

(defn x [[^float x1 ^float y1 ^float z1]]
  x1)

(defn y [[^float x1 ^float y1 ^float z1]]
  y1)

(defn z [[^float x1 ^float y1 ^float z1]]
  z1)

(defn reflect [v n]
  (let [m (* n (clj/* (dot v n) 2))]
    (- v m)))

(defn refract [uv n eta-ratio]
  (let [cos-theta (min (clj/- (dot uv n)) 1)
        r-out-perp (* (+ uv (* n cos-theta)) eta-ratio)
        r-out-parallel (* n (clj/- (Math/sqrt (Math/abs (clj/- 1 (length-squared r-out-perp))))))]
    (+ r-out-perp r-out-parallel)))

(defn near-zero [[x y z]]
  (and (< x 1e-8) (< y 1e-8) (< z 1e-8)))