(require '[clojure.string :refer [split]])

(defn cost-a [n] (map (fn [x] (Math/abs (- x n)))))
(defn solve-a [xs]
  (let [xs (into [] (sort xs)) m (xs (/ (count xs) 2))]
    (transduce (cost-a m) + 0 xs)))

(defn cost-b [n] (map (fn [x] (let [d (Math/abs (- x n))] (int (* (/ d 2) (inc d)))))))
(defn solve-b [xs]
  (let [m (int (/ (reduce + xs) (count xs)))]
    (transduce (cost-b m) + 0 xs)))

(let [ln (first (line-seq (java.io.BufferedReader. *in*)))
      xs (sequence (map (fn [x] (Integer. x))) (split ln #","))]
  (println "Part A:" (solve-a xs))
  (println "Part B:" (solve-b xs)))
