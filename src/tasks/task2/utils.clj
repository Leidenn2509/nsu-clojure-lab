(ns tasks.task2.utils)

(defn area [a b h]
    (/ (* (+ a b) h) 2.0))

(defn farea-ab [f a b] (area (f a) (f b) (- b a)))

(defn farea [f x dx] (area (f x) (f (- x dx)) dx))

(defn farea-n [f n dx] (area (f (* n dx)) (f (* (dec n) dx)) dx))

(defn nearest-n [x dx]
    (int (/ x dx)))