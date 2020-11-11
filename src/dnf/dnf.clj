(ns dnf.dnf
    (:use dnf.api)
    (:use dnf.parser)
    (:use dnf.printer))

(defn apply-recur
    "Apply function `f` to expr and to arguments of result"
    [f expr]
    (let [new-expr (f expr)]
        (cond
            (nil? new-expr) nil
            (or (variable? new-expr) (constant? new-expr)) new-expr
            :else (update-args new-expr (map #(apply-recur f %) (args new-expr))))))

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
                (constant? arg) (if (= arg C-TRUE) C-FALSE C-TRUE)
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


(defn- args-contains-const?
    [expr const]
    (some #(and (constant? %)
                (= const %))
          (args expr)))

(defn simplify
    "Simplify conjunction and disjunction if there constant"
    [expr]
    (let [expr (decompose expr)]
        (if (leaf? expr)
            expr
            (cond
                (and (dnf-and? expr)
                     (args-contains-const? expr C-FALSE)) C-FALSE
                (and (dnf-and? expr)
                     (args-contains-const? expr C-TRUE)) (apply dnf-and (filter #(not (= % C-TRUE)) (args expr)))
                (and (dnf-or? expr)
                     (args-contains-const? expr C-TRUE)) C-TRUE
                (and (dnf-or? expr)
                     (args-contains-const? expr C-FALSE)) (apply dnf-or (filter #(not (= % C-FALSE)) (args expr)))
                :else expr)
            )))


(defn- var-or-nvar? [expr]
    (or (variable? expr)
        (and (dnf-not? expr) (variable? (first (args expr))))))

(defn dnf? [expr]
    (let [expr (decompose expr)]
        (or (nil? expr)
            (constant? expr)
            (var-or-nvar? expr)
            (and (dnf-and? expr)
                 (every? #(var-or-nvar? %) (args expr)))
            (and (dnf-or? expr)
                 (every? #(or (dnf-and? %) (var-or-nvar? %)) (args expr))
                 (every? true?
                         (map (fn [arg] (every? #(var-or-nvar? %)
                                                (args arg)))
                              (filter #(not (leaf? %)) (args expr))))))))

(defn to-dnf [expr]
    (->> expr
         (apply-recur remove-impl)
         (apply-recur op-brackets)
         (apply-recur distributive-prop)
         (apply-recur simplify)))

(defn substitute
    "Substitute variables to expression"
    [expr var-map]
    (apply-recur simplify
                 (cond
                     (variable? expr) (let [key (first (args expr))]
                                          (if (contains? var-map key)
                                              (constant (get var-map key))
                                              expr))
                     (constant? expr) expr
                     :else (dnf-of-type expr (map (fn [expr] (substitute expr var-map)) (args expr))))))