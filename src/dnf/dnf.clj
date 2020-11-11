(ns dnf.dnf
    (:use dnf.api)
    (:use dnf.parser)
    (:use dnf.printer))

(defn substitute
    "Substitute variables to expression"
    [expr variables]
    (cond
        (variable? expr) (let [const (some (fn [[v c]] (if (same-variables? v expr) c false)) variables)]
                             (if const const expr))
        (constant? expr) expr
        :else (cons (first expr) (map (fn [expr] (substitute expr variables)) (args expr)))))

(defn apply-rule [expr [pred transformer]]
    (if (pred expr)
        (transformer expr)
        expr))

(defn apply-recur [f expr]
    (let [new-expr (f expr)]
        (if (or (variable? new-expr) (constant? new-expr))
            new-expr
            (update-args new-expr (map #(apply-recur f %) (args new-expr))))))

(defn remove-impl
    "Transform all implication in 'not A or B'"
    [expr]
    (if (dnf-impl? expr)
        (dnf-or
            (dnf-not (impl-first-arg expr))
            (impl-second-arg expr))
        expr))

(defn op-brackets
    "Open brackets if not before and, or"
    [expr]
    (if (dnf-not? expr)
        (let [arg (first (args expr))]
            (cond
                (dnf-not? arg) (first (args arg))
                (dnf-and? arg) (apply dnf-or (map (fn [x] (dnf-not x)) (args arg)))
                (dnf-or? arg) (apply dnf-and (map (fn [x] (dnf-not x)) (args arg)))
                :else expr))
        expr))

(defn- dist-prop-helper
    [and-arg or-expr]
    (dnf-or (dnf-and and-arg (first (args or-expr))) (dnf-and and-arg (last (args or-expr)))))

(defn distributive-prop
    "Apply distributive property.
    It is assumed that the number of arguments for infix operations is equal to two."
    [expr]
    (if (dnf-and? expr)
        (let [a (first (args expr)) b (last (args expr))]
            (cond
                (dnf-or? a) (dist-prop-helper b a)
                (dnf-or? b) (dist-prop-helper a b)
                :else expr))
        expr))

(defn dnf [x]
    (->> x
         (apply-recur remove-impl)
         (apply-recur op-brackets)
         (apply-recur distributive-prop)))
