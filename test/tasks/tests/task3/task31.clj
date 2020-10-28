(ns tasks.tests.task3.task31
    (:require [clojure.test :refer :all])
    (:use tasks.task3.task31))

;(deftest filter-test
;    (testing "Test parallel-filter output"
;        (let [coll (take 20 (iterate inc -10))]
;            (is (= (parallel-filter even? 3 coll) (filter even? coll)))
;            (is (= (parallel-filter neg? 3 coll) (filter neg? coll)))
;            (is (= (parallel-filter #(= % 0) 5 coll) (filter #(= % 0) coll))))))


(deftest filter-time
    (testing "Compare time for parallel-filter and filter"
        (letfn [(heavy-even? [x] (Thread/sleep 10) (even? x))]
            (println "(filter heavy-even? (take 10 (iterate inc -5)))")
            (time (doall (filter heavy-even? (take 10 (iterate inc -5)))))
            (println "(parallel-filter heavy-even? 2 (take 10 (iterate inc -5)))")
            (time (doall (parallel-filter heavy-even? 2 (take 10 (iterate inc -5))))))))
