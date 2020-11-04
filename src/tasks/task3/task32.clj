(ns tasks.task3.task32
    (:use tasks.task3.task31))

(defn lazy-parallel-filter [pred sublist-size thread-count coll]
    (->> (break-coll (* sublist-size thread-count) coll)
         (map (fn [list] (->> (break-coll sublist-size list)
                              (map (fn [x] (future (doall (filter pred x)))))
                              (doall)
                              (map deref))))
         (flatten)))
