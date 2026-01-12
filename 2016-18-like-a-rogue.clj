(defn advance [xs]
  (loop [rs (transient []) idx 2 a 1 b (xs 0) c (xs 1)]
    (if (not= (count rs) (count xs))
      (recur (conj! rs (if (= a c) 1 0)) (inc idx) b c (get xs idx 1))
      (persistent! rs))))

(defn solve [xs n]
  (loop [xs xs sum 0 t 0]
    (if (< t n)
      (recur (advance xs) (reduce + sum xs) (inc t))
      sum)))

(let [in (first (line-seq (java.io.BufferedReader. *in*)))
      xs (into [] (map (fn [c] (if (= c \.) 1 0))) in)]
  (println "Part A:" (solve xs 40))
  (println "Part B:" (solve xs 400000)))
