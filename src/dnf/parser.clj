(ns dnf.parser
    (:use dnf.api))


(defn lexer [string]
    (map #(cond
              (= % \() ::open
              (= % \)) ::close
              (= % \1) ::true
              (= % \0) ::false
              (= % \*) ::and
              (= % \+) ::or
              (= % \>) ::impl
              (= % \-) ::not
              :else (list ::symbol (keyword (str %)))) string))

(defn split-in [pred? stack]
    [(take-while pred? stack) (drop-while pred? stack)])

(defn list-or-first [coll]
    (if (= (count coll) 1) (first coll) coll))

(defn conj-list [coll xs]
    (conj coll (list-or-first xs)))

(defn rpn [tokens res stack]
    (let [token (first tokens)]
        (cond
            (= token ::true) (rpn (rest tokens) (conj res token) stack)
            (= token ::false) (rpn (rest tokens) (conj res token) stack)
            (= token ::not) (rpn (rest tokens) res (cons token stack))
            (= token ::open) (rpn (rest tokens) res (cons token stack))
            (= token ::close) (let [[pop r] (split-in #(not (= % ::open)) stack)]
                                  (rpn
                                      (rest tokens)
                                      (if (empty? pop) res (vec (concat res pop)))
                                      (rest r)))
            (= token ::or) (let [[pop r] (split-in #(or (= % ::not) (= % ::and) (= % ::or)) stack)]
                               (rpn
                                   (rest tokens)
                                   (if (empty? pop) res (vec (concat res pop)))
                                   (cons token r)))         ;;rest?
            (= token ::and) (let [[pop r] (split-in #(or (= % ::not) (= % ::and)) stack)]
                                (rpn
                                    (rest tokens)
                                    (if (empty? pop) res (vec (concat res pop)))
                                    (cons token r)))        ;;rest?
            (= token ::impl) (let [[pop r] (split-in #(or (= % ::not) (= % ::and) (= % ::impl)) stack)]
                                 (rpn
                                     (rest tokens)
                                     (if (empty? pop) res (vec (concat res pop)))
                                     (cons token r)))       ;;rest?
            (= (first token) ::symbol) (rpn (rest tokens) (conj res token) stack)
            :else (if (empty? stack) res (concat res stack))
            )))

(defn parse
    ([string] (parse (rpn (lexer string) [] `()) `()))
    ([rpn stack] (let [token (first rpn)
                       r (rest rpn)]
                     (cond
                         (= token ::true) (parse r (cons (constant true) stack))
                         (= token ::false) (parse r (cons (constant false) stack))
                         (= token ::not) (parse r (cons (dnf-not (first stack)) (rest stack)))
                         (= token ::or) (parse r (cons (dnf-or (second stack) (first stack)) (drop 2 stack))) ;; apply?
                         (= token ::and) (parse r (cons (dnf-and (second stack) (first stack)) (drop 2 stack)))
                         (= token ::impl) (parse r (cons (dnf-impl (second stack) (first stack)) (drop 2 stack)))
                         (= (first token) ::symbol) (parse r (cons (variable (second token)) stack))
                         :else (first stack)))))