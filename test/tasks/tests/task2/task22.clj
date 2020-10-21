(ns tasks.tests.task2.task22
    (:require [clojure.test :refer :all])
    (:use tasks.task2.task22))

(defn xxs [x] (/ (* x (* x x)) 3.0))

(defn almost-equal [a b epsilon]
    (< (- (max a b) (min a b)) epsilon))

(deftest task22-test-simple
    (testing "task22 test simple function"
        (let [f (integr2 (fn [_] 2))]
            (is (= (f 1) 2.0))
            (is (= (f 2) 4.0))
            (is (= (f 5) 10.0)))
        (let [f (integr2 (fn [x] x))]
            (is (= (f 1) 0.5))
            (is (= (f 2) 2.0))
            (is (= (f 5) 12.5)))
        (let [f (integr2 (fn [x] (* x x))) epsilon 0.001]
            (is (almost-equal (f 1) (xxs 1) epsilon))
            (is (almost-equal (f 2) (xxs 2) epsilon))
            (is (almost-equal (f 5) (xxs 5) epsilon)))))

(deftest task22-time
    (let [f (integr2 (fn [_] (Thread/sleep 1) 1))]
        (println "(f 5)")
        (time (f 5))
        (println "(f 5)")
        (time (f 5))
        (println "(f 10)")
        (time (f 10))
        (println "(f 11)")
        (time (f 11))))