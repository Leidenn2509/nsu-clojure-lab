(ns tasks.task2.task21)

;(def dx 0.5)
;(def dx 0.25)
;(def dx 0.125)
;(def dx 0.0625)
;(def dx 0.03125)
(def dx 0.015625)
;(def dx 0.0078125)
;(def dx 0.00390625)
;(def dx 0.001953125)
;(def dx 0.0009765625)

(defn area [a b h]
    (/ (* (+ a b) h) 2.0))

(defn farea [f x dx] (area (f x) (f (- x dx)) dx))

(def di (memoize (fn [f x]
                     (if (> x 0)
                         (+ (di f (- x dx)) (farea f x dx))
                         0))))

(defn integr [f] (memoize (fn [x] (di f x))))

