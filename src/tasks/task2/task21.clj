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

(defn farea-ab [f a b] (area (f a) (f b) (- b a)))

(defn farea [f x dx] (area (f x) (f (- x dx)) dx))

(defn farea-n [f n dx] (area (f (* n dx)) (f (* (dec n) dx)) dx))

(defn nearest-n [x dx]
    (int (/ x dx)))

(def di (memoize (fn [f n]
                     (if (> n 0)
                         (+ (di f (dec n)) (farea-n f n dx))
                         0))))

(defn integr [f]
    (fn [x] (let [n (nearest-n x dx)]
                (+
                    (di f n)
                    (area (f x) (f (* dx n)) (- x (* dx n)))))
        ))
