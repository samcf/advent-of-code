(defn index-of [s n]
  (->> (partition n 1 s)
       (take-while #(< (count (set %)) n))
       (count)
       (+ n)))

(let [ln (first (line-seq (java.io.BufferedReader. *in*)))]
  (println "Part A:" (index-of ln 4))
  (println "Part B:" (index-of ln 14)))
