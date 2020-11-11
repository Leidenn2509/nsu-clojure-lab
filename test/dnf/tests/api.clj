(ns dnf.tests.api
    (:require [clojure.test :refer :all])
    (:use dnf.api))


(deftest api-test
    (testing "Test variable api"
        (is (variable? (variable :x)))
        (is (= :x (variable-name (variable :x))))
        (is (same-variables?
                (variable :x)
                (variable :x)))
        (is (not (same-variables?
                     (variable :x)
                     (variable :y)))))
    (testing "Test constant api"
        (is (constant? (constant true)))
        (is (= false (constant-value (constant false)))))
    (testing "Test 'and' api"
        (is (dnf-and? (dnf-and
                          (constant false)
                          (constant true))))
        (is (dnf-and? (dnf-and
                          (constant false)
                          (variable :x))))
        (is (not (dnf-and?
                     (dnf-and (constant true)))))
        (is (let [args (args (dnf-and (variable :x) (variable :y)))]
                (and
                    (same-variables? (first args) (variable :x))
                    (same-variables? (second args) (variable :y))))))
    (testing "Test 'or' api"
        (is (dnf-or? (dnf-or
                         (constant false)
                         (constant true))))
        (is (dnf-or? (dnf-or
                         (constant false)
                         (variable :x))))
        (is (not (dnf-or?
                     (dnf-or (constant true)))))
        (is (let [args (args (dnf-or (variable :x) (variable :y)))]
                (and
                    (same-variables? (first args) (variable :x))
                    (same-variables? (second args) (variable :y))))))
    (testing "Test 'impl' api"
        (is (dnf-impl? (dnf-impl (variable :x) (constant false))))
        (is (let [impl (dnf-impl (variable :x) (variable :y))]
                (and
                    (same-variables? (impl-first-arg impl) (variable :x))
                    (same-variables? (impl-second-arg impl) (variable :y))))))
    (testing "Test utils functions"
        (is (same-type? (variable :x) (variable :y)))
        (is (not (same-type? (variable :x) (constant :y))))
        (is (same-expr-strict? (constant true) (constant true)))
        (is (not (same-expr-strict? (constant true) (constant false))))
        (is (same-expr-strict? (variable :x) (variable :x)))
        (is (not (same-expr-strict? (variable :x) (variable :y))))
        (is (same-expr-strict?
                (dnf-or (variable :x) (constant true))
                (dnf-or (variable :x) (constant true))))))
