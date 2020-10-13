(ns tasks.task1.task11
    (:use tasks.utils))


(defn add-char-to-string-list [s chars]
    (cond
        (= (count chars) 0) (list)
        (> (count chars) 0) (if (= s (first-char (first chars)))
                                (add-char-to-string-list s (rest chars))
                                (concat
                                    (list (str s (first chars)))
                                    (add-char-to-string-list s (rest chars))))))

(defn concat-stringlist-to-stringlist [chars1 chars2]
    (cond (= (count chars2) 0) chars1
          (> (count chars1) 0) (concat
                                   (add-char-to-string-list (first chars1) chars2)
                                   (concat-stringlist-to-stringlist (rest chars1) chars2))

          (= (count chars1) 0) (list)))

(defn task11
    ([chars n]
     (if (= n 1)
         chars
         (task11 chars n (list))))
    ([chars n res]
     (if (= n 0)
         res
         (task11 chars (dec n)  (concat-stringlist-to-stringlist chars res)))))
