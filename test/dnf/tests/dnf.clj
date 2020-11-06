(ns dnf.tests.dnf
    (:require [clojure.test :refer :all])
    (:use dnf.dnf)
    (:use dnf.api)
    (:use dnf.parser))

(defn- compare-transform-expr [func string1 string2]
    (same-expr? (func (parse string1)) (parse string2)))

(deftest remove-impl-test
    (testing "Test 'remove-impl'"
        (is (compare-transform-expr remove-impl "a>b" "-a+b"))
        (is (compare-transform-expr remove-impl "a>b>c" "-(-a+b)+c")) ;; ????????
        ))


(defn- substitute- [variables]
    (fn [expr] (substitute expr variables)))

(deftest substitute-test
    (testing "Test substitution"
        (is (compare-transform-expr (substitute- [[(variable :x) (constant true)]]) "1" "1"))
        (is (compare-transform-expr (substitute- [[(variable :x) (constant true)]]) "x" "1"))
        (is (compare-transform-expr (substitute- [[(variable :x) (constant true)]]) "1+x" "1+1"))
        (is (compare-transform-expr (substitute- [[(variable :x) (constant true)]]) "y>(1+x)" "y>(1+1)"))
        (is (compare-transform-expr (substitute- [[(variable :x) (constant true)]
                                                  [(variable :y) (constant false)]]) "y>(1+x)" "0>(1+1)"))
        (is (compare-transform-expr (substitute- [[(variable :x) (variable :y)]]) "y>(1+x)" "y>(1+y)"))))