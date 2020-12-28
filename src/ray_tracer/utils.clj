(ns ray-tracer.utils)

(defn rand-in-range [lo hi]
  (+ (rand (- hi lo)) lo))

(defn in-range? [t t-min t-max]
  (and (> t t-min) (< t t-max)))
