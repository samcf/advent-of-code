(defn cost [x]
  (- (quot x 3) 2))

(defn sums [x]
  (let [y (cost x)]
    (if (> y 0) (+ (sums y) y) 0)))

(let [in (line-seq (java.io.BufferedReader. *in*))]
  (println "Part A:" (transduce (comp (map parse-long) (map cost)) + in))
  (println "Part B:" (transduce (comp (map parse-long) (map sums)) + in)))
