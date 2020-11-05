(ns dnf.dnf
    (:use dnf.api))

(defn remove-impl
    "Transform all implication in 'not A or B'"
    [expr]
    (if (dnf-impl? expr)
        (dnf-or
            (dnf-not (remove-impl (impl-first-arg expr)))
            (remove-impl (impl-second-arg expr)))
        expr))

(defn dnf [x] (dnf-and x))


