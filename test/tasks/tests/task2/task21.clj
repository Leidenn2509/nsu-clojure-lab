(ns tasks.tests.task2.task21
    (:require [clojure.test :refer :all])
    (:use tasks.task2.task21))

(defn xxs [x] (/ (* x (* x x)) 3.0))

(defn almost-equal [a b epsilon]
    (< (- (max a b) (min a b)) epsilon))

(deftest task21-test-simple
    (testing "task21 test simple function"
        (let [f (integr (fn [_] 2))]
            (is (= (f 1) 2.0))
            (is (= (f 2) 4.0))
            (is (= (f 10) 20.0)))
        (let [f (integr (fn [x] x))]
            (is (= (f 1) 0.5))
            (is (= (f 2) 2.0))
            (is (= (f 10) 50.0)))
        (let [f (integr (fn [x] (* x x))) epsilon 0.001]
            (is (almost-equal (f 1) (xxs 1) epsilon))
            (is (almost-equal (f 2) (xxs 2) epsilon))
            (is (almost-equal (f 10) (xxs 10) epsilon)))))