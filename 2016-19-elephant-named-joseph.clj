(require '[clojure.math :refer [pow log]])

(defn nearest [x b]
  (long (pow b (quot (log x) (log b)))))

(defn solve [x]
  (inc (* (- x (nearest x 2)) 2)))

(defn solve' [x]
  (inc (- x (inc (nearest x 3)))))

(let [in (line-seq (java.io.BufferedReader. *in*))
      x  (parse-long (first in))]
  (println "Part A:" (solve  x))
  (println "Part B:" (solve' x)))
