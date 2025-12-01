(require '[clojure.math :refer [ceil floor]])

(defn parse [ln]
  (condp re-find ln
    #"R(\d+)" :>> (fn [[_ a]] (+ (parse-long a)))
    #"L(\d+)" :>> (fn [[_ a]] (- (parse-long a)))))

(defn between [x y m]
  (let [a (ceil  (/ (min x y) m))
        b (floor (/ (max x y) m))]
    (int (max 0 (inc (- b a))))))

(defn stop [_ y]
  (if (zero? (mod y 100)) 1 0))

(defn stops [x y]
  (let [n (between x y 100)]
    (if (zero? (mod x 100))
      (max (dec n) 0) n)))

(defn solve [f]
  (fn
    ([] {:pos 50 :sum 0})
    ([m] (:sum m))
    ([m o]
     (let [x (:pos m) y (+ x o)]
       (update (assoc m :pos y) :sum + (f x y))))))

(let [in (line-seq (java.io.BufferedReader. *in*))]
  (println "Part A:" (transduce (map parse) (solve stop) in))
  (println "Part B:" (transduce (map parse) (solve stops) in)))
