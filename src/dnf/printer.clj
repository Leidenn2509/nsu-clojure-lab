(ns dnf.printer
    (:use dnf.api))

(defn dnf-print [expr]
    (cond
        (variable? expr) (name (first (args expr)))
        (constant? expr) (str (first (args expr)))
        (dnf-or? expr) (str "(" (reduce #(str %1 "+" (dnf-print %2)) (dnf-print (first (args expr))) (rest (args expr))) ")")
        (dnf-and? expr) (str "(" (reduce #(str %1 "*" (dnf-print %2)) (dnf-print (first (args expr))) (rest (args expr))) ")")
        :else (throw "TODO")))