(ns tasks.task2.task22
    (:use tasks.task2.utils))

(def dxx 0.015625)


(defn iterate-integr [f]
    (map first (iterate
                   (fn [[sum step]]
                       [(+ sum (farea-n f (inc step) dxx)) (inc step)])
                   [0 0])))

(def memoize-iterate-integr (memoize (fn [f] (iterate-integr f))))

(defn integr2 [f]
    (fn [x] (let [n (nearest-n x dxx)]
                (+ (nth (memoize-iterate-integr f) n) (farea-ab f (* dxx n) x)))))