(ns tasks.task2.task22
    (:use tasks.task2.utils))

(def dxx 0.015625)


(defn iterate-integr [f]
    (map first (iterate
                   (fn [[sum step]]
                       [(+ sum (farea-n f (inc step) dxx)) (inc step)])
                   [0 0])))

(defn remember-seq [f, seq] (fn [x] (let [n (nearest-n x dxx)]
                                        (+
                                            (nth seq n)
                                            (if (= (rem x dxx) 0.0)
                                                0.0
                                                (farea-ab f (* dxx n) x))))))

(defn integr2 [f]
    (remember-seq f (iterate-integr f)))