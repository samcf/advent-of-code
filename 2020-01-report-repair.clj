(let [xs (->> (java.io.BufferedReader. *in*) (line-seq) (sequence (map (fn [x] (Integer. x)))))]
  (println "Part A:" (first (for [a xs b xs      :when (= (+ a b)   2020)] (* a b))))
  (println "Part B:" (first (for [a xs b xs c xs :when (= (+ a b c) 2020)] (* a b c)))))
