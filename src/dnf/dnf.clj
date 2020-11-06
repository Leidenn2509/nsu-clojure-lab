(ns dnf.dnf
    (:use dnf.api)
    (:use dnf.parser))

(defn substitute
    "Substitute variables to expression"
    [expr variables]
    (cond
        (variable? expr) (let [const (some (fn [[v c]] (if (same-variables? v expr) c false)) variables)]
            (if const const expr))
        (constant? expr) expr
        :else (cons (first expr) (map (fn [expr] (substitute expr variables)) (args expr)))))



(defn remove-impl
    "Transform all implication in 'not A or B'"
    [expr]
    (if (dnf-impl? expr)
        (dnf-or
            (dnf-not (remove-impl (impl-first-arg expr)))
            (remove-impl (impl-second-arg expr)))
        expr))

(defn dnf [x] (dnf-and x))
