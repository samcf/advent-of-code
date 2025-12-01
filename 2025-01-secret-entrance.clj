(require '[clojure.math :refer [ceil floor]])

(defn parse [ln]
  (condp re-find ln
    #"R(\d+)" :>> (fn [[_ a]] (parse-long a))
    #"L(\d+)" :>> (fn [[_ a]] (* (parse-long a) -1))))

(defn between [a b s]
  (let [x (ceil  (/ (min a b) s))
        y (floor (/ (max a b) s))]
    (int (max 0 (inc (- y x))))))

(defn stop [_ x]
  (if (zero? (mod x 100)) 1 0))

(defn stops [m x]
  (let [n (between (:pos m) x 100)]
    (if (zero? (mod (:pos m) 100))
      (max (dec n) 0) n)))

(defn solve [f]
  (fn
    ([] {:pos 50 :sum 0})
    ([m] (:sum m))
    ([m x]
     (let [y (+ (:pos m) x)]
       (update (assoc m :pos y) :sum + (f m y))))))

(let [in (line-seq (java.io.BufferedReader. *in*))]
  (println "Part A:" (transduce (map parse) (solve stop) in))
  (println "Part B:" (transduce (map parse) (solve stops) in)))
