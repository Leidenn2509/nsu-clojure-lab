(ns dnf.api)

;; Constant and Variables

(defn constant
    "Constructor for constant"
    [num] (list ::const num))

(defn constant?
    "Check if expr is constant"
    [expr] (= (first expr) ::const))

(defn constant-value
    "Get constant value"
    [expr] (second expr))

(defn variable
    "Constructor for variable"
    [name]
    {:pre [(keyword? name)]}
    (list ::var name))

(defn variable?
    "Check if type expr is variable"
    [expr] (= (first expr) ::var))

(defn variable-name
    "Get variable name"
    [var] (second var))

(defn same-variables?
    "Check if two variable are same"
    [v1 v2]
    (and
        (variable? v1)
        (variable? v2)
        (= (variable-name v1)
           (variable-name v2))))

;; And, Or, Not, Impl

(defn args
    "Get arguments of operation"
    [expr] (rest expr))

(defn dnf-and
    "Constructor for conjunctions operation"
    [expr & rest]
    (if (empty? rest)
        expr
        (cons ::and (cons expr rest))))

(defn dnf-and?
    "Check if expression is conjunction"
    [expr] (= (first expr) ::and))


(defn dnf-or
    "Constructor for disjunction operation"
    [expr & rest]
    (if (empty? rest)
        expr
        (cons ::or (cons expr rest))))

(defn dnf-or?
    "Check if expression is disjunction"
    [expr] (= (first expr) ::or))


(defn dnf-not
    "Constructor for not operation"
    [expr] (list ::not expr))

(defn dnf-not?
    "Check if expression is 'not'"
    [expr] (= (first expr) ::not))


(defn dnf-impl
    "Constructor for implication operation"
    [expr-x expr-y] (list ::impl expr-x expr-y))

(defn dnf-impl?
    "Check if expression is implication"
    [expr] (= (first expr) ::impl))

(defn impl-first-arg
    "Get first argument of implication"
    [impl]
    {:pre (dnf-impl? impl)}
    (first (rest impl)))

(defn impl-second-arg
    "Get second argument of implication"
    [impl]
    {:pre (dnf-impl? impl)}
    (last (rest impl)))

;; Utils

(defn same-type?
    "Check that expr1 and expr2 have same type"
    [expr1 expr2] (= (first expr1) (first expr2)))

(defn same-expr?
    "Check if two expressions is same"
    [expr1 expr2]
    (if (not (same-type? expr1 expr2))
        false
        (cond
            (variable? expr1) (same-variables? expr1 expr2)
            (constant? expr1) (= (constant-value expr1) (constant-value expr2))
            :else (->> (map #(same-expr? %1 %2) (args expr1) (args expr2))
                 (every? true?)))))