(ns tasks.core
    (:use tasks.task11))


(defn test-task11 []
    (assert (= (tasks.task11/task11 `("a" "b" "c") 0) `()))
    (assert (= (tasks.task11/task11 `("a" "b" "c") 1) `("a" "b" "c")))
    (assert (= (tasks.task11/task11 `("a" "b" "c") 2) `("ab" "ac" "ba" "bc" "ca" "cb")))
    (assert (= (tasks.task11/task11 `("a" "b" "c") 3) `("aba" "abc" "aca" "acb" "bab" "bac" "bca" "bcb" "cab" "cac" "cba" "cbc")))
    (assert (= (tasks.task11/task11 `() 1) `())))

(defn -main
    []
    (test-task11))
