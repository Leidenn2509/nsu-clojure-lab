(ns tasks.task3.task32
    (:use tasks.task3.task31))

(defn lazy-parallel-filter [pred sublist-size thread-count coll]
    (->> (partition (* sublist-size thread-count) coll)
         (map (fn [list] (->> (partition sublist-size list)
                              (map (fn [x] (future (doall (filter pred x)))))
                              (doall)
                              (map deref)
                              (println))))
         (flatten)))
