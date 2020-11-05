(ns dnf.tests.dnf
    (:require [clojure.test :refer :all])
    (:use dnf.dnf)
    (:use dnf.api))

(deftest remove-impl-test
    (testing "Test 'remove-impl'"
        (is (same-expr?
                (dnf-or (dnf-not (variable :x)) (constant true))
                (remove-impl (dnf-impl (variable :x) (constant true)))))
        (is (same-expr?
                (dnf-or (dnf-not (dnf-or (dnf-not (variable :x )) (variable :y))) (constant true))
                (remove-impl (dnf-impl (dnf-impl (variable :x) (variable :y)) (constant true)))))))