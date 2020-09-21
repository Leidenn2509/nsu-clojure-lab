(ns tasks.core-test
  (:require [clojure.test :refer :all]
            [tasks.core :refer :all]))

(deftest task11-test
  (testing "task11"
      (is (= (tasks.task11/task11 `("a" "b" "c") 0) `()))
      (is (= (tasks.task11/task11 `("a" "b" "c") 1) `("a" "b" "c")))
      (is (= (tasks.task11/task11 `("a" "b" "c") 2) `("ab" "ac" "ba" "bc" "ca" "cb")))
      (is (= (tasks.task11/task11 `("a" "b" "c") 3) `("aba" "abc" "aca" "acb" "bab" "bac" "bca" "bcb" "cab" "cac" "cba" "cbc")))
      (is (= (tasks.task11/task11 `() 1) `()))))

(deftest task12-test
    (testing "task12"
        (is (= (tasks.task12/task12 `("a" "b" "c") 0) `()))
        (is (= (tasks.task12/task12 `("a" "b" "c") 1) `("a" "b" "c")))
        (is (= (tasks.task12/task12 `("a" "b" "c") 2) `("ab" "ac" "ba" "bc" "ca" "cb")))
        (is (= (tasks.task12/task12 `("a" "b" "c") 3) `("aba" "abc" "aca" "acb" "bab" "bac" "bca" "bcb" "cab" "cac" "cba" "cbc")))
        (is (= (tasks.task12/task12 `() 1) `()))))