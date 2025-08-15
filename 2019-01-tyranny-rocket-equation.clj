(defn step [x]
  (- (quot x 3) 2))

(defn cost [x]
  (let [y (step x)]
    (if (> y 0) (+ (cost y) y) 0)))

(let [in (line-seq (java.io.BufferedReader. *in*))]
  (println "Part A:" (transduce (comp (map parse-long) (map step)) + in))
  (println "Part B:" (transduce (comp (map parse-long) (map cost)) + in)))
