(ns dnf.tests.parser
  (:require [clojure.test :refer :all])
  (:use dnf.parser)
  (:use dnf.api))

(deftest parser-test
    (testing "Test parser func"
        (is (same-expr-strict? (parse "a") (variable :a)))
        (is (same-expr-strict? (parse "1") (constant true)))
        (is (same-expr-strict? (parse "0") (constant false)))
        (is (same-expr-strict? (parse "(a)") (variable :a)))
        (is (same-expr-strict? (parse "a+1") (dnf-or (variable :a) (constant true))))
        (is (same-expr-strict? (parse "a*1") (dnf-and (variable :a) (constant true))))
        (is (same-expr-strict? (parse "a>1") (dnf-impl (variable :a) (constant true))))
        (is (same-expr-strict? (parse "-a") (dnf-not (variable :a))))
        (is (same-expr-strict? (parse "(a+b)*1") (dnf-and (dnf-or (variable :a) (variable :b)) (constant true))))
        (is (same-expr-strict? (parse "a+b*1") (dnf-or (variable :a) (dnf-and (variable :b) (constant true)))))
        (is (same-expr-strict? (parse "a>b>c") (dnf-impl (variable :a) (dnf-impl (variable :b) (variable :c)))))
        (is (same-expr-strict? (parse "a>-b>c") (dnf-impl (variable :a) (dnf-impl (dnf-not (variable :b)) (variable :c)))))
        (is (same-expr-strict? (parse "a>(b>c)") (dnf-impl (variable :a) (dnf-impl (variable :b) (variable :c)))))
        (is (same-expr-strict? (parse "a>-(b>c)") (dnf-impl (variable :a) (dnf-not (dnf-impl (variable :b) (variable :c))))))
        (is (same-expr-strict? (parse "a+-c") (dnf-or (variable :a) (dnf-not (variable :c)))))
        (is (same-expr-strict? (parse "a+b*1+c") (dnf-or (dnf-or (variable :a) (dnf-and (variable :b) (constant true))) (variable :c))))))