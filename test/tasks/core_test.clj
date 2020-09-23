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
      ;(is (thrown? StackOverflowError (tasks.task11 `("a" "b" "c") 15)))




(deftest task12-test
    (testing "task12"
        (is (= (tasks.task12/task12 `("a" "b" "c") 0) `()))
        (is (= (tasks.task12/task12 `("a" "b" "c") 1) `("a" "b" "c")))
        (is (= (tasks.task12/task12 `("a" "b" "c") 2) `("ac" "ab" "bc" "ba" "cb" "ca")))
        (is (= (tasks.task12/task12 `("a" "b" "c") 3) `("aca" "acb" "aba" "abc" "bca" "bcb" "bab" "bac" "cba" "cbc" "cab" "cac")))
        (is (= (tasks.task12/task12 `() 1) `()))))

(deftest task14-test
    (testing "task11"
        (is (= (tasks.task14/task14 `("a" "b" "c") 0) `()))
        (is (= (tasks.task14/task14 `("a" "b" "c") 1) `("a" "b" "c")))
        (is (= (tasks.task14/task14 `("a" "b" "c") 2) `("ab" "ac" "ba" "bc" "ca" "cb")))
        (is (= (tasks.task14/task14 `("a" "b" "c") 3) `("aba" "abc" "aca" "acb" "bab" "bac" "bca" "bcb" "cab" "cac" "cba" "cbc")))
        (is (= (tasks.task14/task14 `() 1) `()))))