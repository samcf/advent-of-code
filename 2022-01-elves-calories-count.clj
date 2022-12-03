(let [lns (line-seq (java.io.BufferedReader. *in*))
      sum (into (sorted-set-by >)
                (comp (map #(if (= % "") 0 (Integer. %)))
                      (partition-by zero?)
                      (map #(apply + %)))
                lns)]
  (println "Part A:" (first sum))
  (println "Part B:" (apply + (take 3 sum))))
