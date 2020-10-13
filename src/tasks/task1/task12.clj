(ns tasks.task1.task12
    (:use tasks.utils))

(defn add-char-to-string-list-recur
    ([s strings] (add-char-to-string-list-recur s strings (list)))
    ([s strings res] (if (= (count strings) 0)
                         res
                         (if (= s (first-char (first strings)))
                             (recur s (rest strings) res)
                             (recur s (rest strings) (cons (.concat s (first strings)) res))))))


(defn concat-stringlist-to-stringlist-recur
    ([strings1 strings2] (concat-stringlist-to-stringlist-recur strings1 strings2 (list)))
    ([strings1 strings2 res] (cond (= (count strings2) 0) strings1
                                   (> (count strings1) 0) (recur
                                                              (rest strings1)
                                                              strings2
                                                              (concat
                                                                  res
                                                                  (add-char-to-string-list-recur (first strings1) strings2)))
                                   (= (count strings1) 0) res)))


(defn task12
    ([chars n]
     (if (= n 1)
         chars
         (task12 chars n (list))))
    ([chars n res]
     (if (= n 0)
         res
         (recur chars (dec n) (concat-stringlist-to-stringlist-recur chars res)))))
