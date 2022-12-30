(let [xs (into (sorted-set) (map #(Integer. %)) (line-seq (java.io.BufferedReader. *in*)))]
  (println "Part A:" (first (for [a xs b xs      :when (= (+ a b)   2020)] (* a b))))
  (println "Part B:" (first (for [a xs b xs c xs :when (= (+ a b c) 2020)] (* a b c)))))
