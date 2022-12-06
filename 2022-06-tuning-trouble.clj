(defn index-of [s n]
  (->> (partition n 1 s) (map-indexed (fn [idx part] [idx (set part)])) (filter (fn [[_ s]] (= (count s) n))) (ffirst) (+ n)))

(let [ln (first (line-seq (java.io.BufferedReader. *in*)))]
  (println "Part A:" (index-of ln 4))
  (println "Part B:" (index-of ln 14)))
