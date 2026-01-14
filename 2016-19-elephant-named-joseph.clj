(require '[clojure.math :refer [pow log]])

(defn nearest [x b]
  (long (pow b (quot (log x) (log b)))))

(let [in (line-seq (java.io.BufferedReader. *in*))
      x  (parse-long (first in))]
  (println "Part A:" (inc (* 2 (- x (nearest x 2)))))
  (println "Part B:" (inc (- x (inc (nearest x 3))))))
