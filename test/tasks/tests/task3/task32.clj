(ns tasks.tests.task3.task32
    (:require [clojure.test :refer :all])
    (:use tasks.task3.task32))

(deftest lazy-filter-test
    (testing "Test lazy-parallel-filter output"
        (let [coll (take 20 (iterate inc -10))]
            (is (= (lazy-parallel-filter even? 4 3 coll) (filter even? coll)))
            (is (= (lazy-parallel-filter neg? 2 3 coll) (filter neg? coll)))
            (is (= (lazy-parallel-filter #(= % 0) 3 5 coll) (filter #(= % 0) coll))))))

(deftest lazy-filter-infinite
    (testing "Test lazy-parallel-filter with infinity seq"
        (let [lazy-filter (lazy-parallel-filter even? 5 6 (iterate inc 0))
              def-filter (filter even? (iterate inc 0))]
            (is (= (take 10 lazy-filter) (take 10 def-filter)))
            (is (= (take 10 (drop 10 lazy-filter)) (take 10 (drop 10 def-filter))))
            (is (= (take 150 lazy-filter) (take 150 def-filter))))))

(deftest lazy-filter-time
    (testing "Compare time for lazy-parallel-filter and filter"
        (letfn [(heavy-even? [x] (Thread/sleep 10) (even? x))]
            (println "(filter heavy-even? (take 20 (iterate inc -5)))")
            (time (doall (filter heavy-even? (take 20 (iterate inc -5)))))
            (println "(lazy-parallel-filter heavy-even? 2 6 (take 20 (iterate inc -5)))")
            (time (doall (lazy-parallel-filter heavy-even? 5 6 (take 20 (iterate inc -5)))))
            (println)
            (let [lazy-filter (lazy-parallel-filter heavy-even? 5 6 (iterate inc 0))
                  def-filter (filter heavy-even? (iterate inc 0))
                  n 200]
                (println "(nth filter-res" n ")")
                (time  (nth def-filter n))
                (println "(nth lazy-parallel-filter-res" n ")")
                (time (nth lazy-filter n))))))