(ns tasks.task2.task21
    (:use tasks.task2.utils))

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

(def di (memoize (fn [f n]
                     (if (> n 0)
                         (+ (di f (dec n)) (farea-n f n dx))
                         0))))

(defn integr [f]
    (fn [x] (let [n (nearest-n x dx)]
                (+
                    (di f n)
                    (farea-ab f x (* dx n))))))
