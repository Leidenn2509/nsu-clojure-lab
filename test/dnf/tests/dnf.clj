(ns dnf.tests.dnf
    (:require [clojure.test :refer :all])
    (:use dnf.dnf)
    (:use dnf.api)
    (:use dnf.parser))

(defn- compare-transform-expr [func string1 string2]
    (same-expr? (func (parse string1)) (parse string2)))

(defn- compare-transform-expr-recur [func string1 string2]
    (same-expr? (apply-recur func (parse string1)) (parse string2)))

(deftest remove-impl-test
    (testing "Test 'remove-impl'"
        (is (compare-transform-expr-recur remove-impl "a>b" "-a+b"))
        (is (compare-transform-expr-recur remove-impl "a>b>c" "-(-a+b)+c"))))

(deftest op-brackets-test
    (testing "Test 'op-brackets'"
        (is (compare-transform-expr-recur op-brackets "a" "a"))
        (is (compare-transform-expr-recur op-brackets "-(a+b)" "-a*-b"))
        (is (compare-transform-expr-recur op-brackets "-((-a+b)*b)" "a*(-b)+(-b)"))
        (is (compare-transform-expr-recur op-brackets "-(a*(a+-b))" "-a+(-a)*b"))))

(deftest distributive-prop-test
    (testing "Test 'distributive-prop'"
        (is (compare-transform-expr-recur distributive-prop "a" "a"))
        (is (compare-transform-expr-recur distributive-prop "(a)" "(a)"))
        (is (compare-transform-expr-recur distributive-prop "a+b" "a+b"))
        (is (compare-transform-expr-recur distributive-prop "a*b" "a*b"))
        (is (compare-transform-expr-recur distributive-prop "a*b+1" "a*b+1"))
        (is (compare-transform-expr-recur distributive-prop "a*(b+1)" "a*b+a*1"))
        (is (compare-transform-expr-recur distributive-prop "a*(b+1)" "(a*b)+(a*1)"))
        (is (compare-transform-expr-recur distributive-prop "a*(b+1)" "a*b+a*1"))
        (is (compare-transform-expr-recur distributive-prop "a*(b+(c+d))" "a*c+a*d+a*b"))))

(deftest simplify-test
    (testing "Test 'simplify'"
        (is (compare-transform-expr-recur simplify "a" "a"))
        (is (compare-transform-expr-recur simplify "a*b" "a*b"))
        (is (compare-transform-expr-recur simplify "a+b" "a+b"))
        (is (compare-transform-expr-recur simplify "a+1" ""))
        (is (compare-transform-expr-recur simplify "0+b" "b"))
        (is (compare-transform-expr-recur simplify "0+b+1" ""))
        (is (compare-transform-expr-recur simplify "1*b" "b"))
        (is (compare-transform-expr-recur simplify "a*0" ""))
        (is (compare-transform-expr-recur simplify "a*0*1" ""))
        ))

(defn- substitute- [variables]
    (fn [expr] (substitute expr variables)))

(deftest substitute-test
    (testing "Test substitution"
        (is (compare-transform-expr (substitute- {:x true}) "1" "1"))
        (is (compare-transform-expr (substitute- {:x true}) "x" "1"))
        (is (compare-transform-expr (substitute- {:x true}) "1+x" "1+1"))
        (is (compare-transform-expr (substitute- {:x true}) "y>(1+x)" "y>(1+1)"))
        (is (compare-transform-expr (substitute- {:x true :y false}) "y>(1+x)" "0>(1+1)"))))

(deftest dnf?-test
    (testing "Test 'dnf?'"
        (is (dnf? (parse "")))
        (is (dnf? (parse "a")))
        (is (dnf? (parse "a+b")))
        (is (dnf? (parse "a+-b")))
        (is (dnf? (parse "-a")))
        (is (dnf? (parse "a+(b*c)")))
        (is (dnf? (parse "(b*c)+a")))
        (is (dnf? (parse "a+(b*-c)")))
        (is (not (dnf? (parse "a+-(b*-c)"))))
        (is (not (dnf? (parse "z*a+-(b*-c)"))))
        (is (not (dnf? (parse "a>b"))))
        (is (dnf? (parse "(a*-b)+(-c*d*x)")))
        (is (dnf? (parse "a*b*c")))
        (is (dnf? (parse "(b*c)*a")))
        (is (not (dnf? (parse "1"))))
        (is (not (dnf? (parse "(b*1)*a"))))
        (is (not (dnf? (parse "(a*-b)+(-c*0)"))))
        ))

(deftest to-dnf-test
    (testing "Test 'dnf'"
        (is (dnf? (to-dnf (parse ""))))
        (is (dnf? (to-dnf (parse "a"))))
        (is (dnf? (to-dnf (parse "a+b"))))
        (is (dnf? (to-dnf (parse "a>b"))))
        (is (dnf? (to-dnf (parse "a>b>c"))))
        (is (dnf? (to-dnf (parse "a*b"))))
        (is (dnf? (to-dnf (parse "(a+b)>(c+d>x)"))))
        (is (dnf? (to-dnf (parse "(a+b)>(1+d>x)"))))))