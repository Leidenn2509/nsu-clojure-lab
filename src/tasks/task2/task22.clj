(ns tasks.task2.task22
    (:use tasks.task2.utils))

(def dxx 0.125)
;(def dxx 0.015625)

(defn lazy-integr-for-f
    ([f sum step]
     (lazy-seq
         (if (= step 0)
             (cons 0 (lazy-integr-for-f f 0 (inc step)))
             (let [new-sum (+ sum (area (f (* step dxx)) (f (* (dec step) dxx)) dxx))]
                 (cons
                     new-sum
                     (lazy-integr-for-f f new-sum (inc step))))))))

(def mem-lazy (memoize (fn [f] (lazy-integr-for-f f 0 0))))
;(def mem-lazy (lazy-integr-for-f (fn [x] (println ".") 2) 0 0))

(defn integr2 [f]
    (fn [x] (let [n (nearest-n x dxx)]
                (+ (nth (mem-lazy f) n) (farea-ab f (* dxx n) x)))))
                ;(+ (nth mem-lazy n) (farea-ab f (* dxx n) x)))))

