(ns tasks.utils)


(defn my-map [f coll]
    (apply list (reduce
                    (fn [coll x] (conj coll (f x)))
                    []
                    coll)))

(defn my-filter [pred coll]
    (apply list (reduce (fn [coll x] (if (pred x) (conj coll x) coll))
                        []
                        coll)))

(defn first-char [string]
    (.substring string 0 1))
