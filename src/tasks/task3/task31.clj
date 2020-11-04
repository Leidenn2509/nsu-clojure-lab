(ns tasks.task3.task31)

(def test-coll (iterate inc 0))

(defn heavy-even? [x] (do (Thread/sleep 100) (even? x)))

(defn break-coll [n coll]
    (take-while #(not (empty? %))
                (lazy-seq
                    (cons
                        (take n coll)
                        (break-coll n (drop n coll))))))

(defn parallel-filter [pred n coll]
    (->> coll
         (break-coll n)
         (map #(future (doall (filter pred %))))
         (doall)
         (map deref)
         (apply concat)))

(defn parallel-filter-by-threads [pred n coll]
    (parallel-filter pred (/ (count coll) n) coll))