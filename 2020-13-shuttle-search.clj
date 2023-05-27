(defn dist-kv [st]
  (fn [iv]
    [(- (* (inc (int (/ st iv))) iv) st) iv]))

(let [[st xs] (line-seq (java.io.BufferedReader. *in*))
      tm (Integer. st)
      xs (sequence (map (fn [x] (Integer. x))) (re-seq #"\d+" xs))
      vs (into (sorted-map) (map (dist-kv tm)) xs)]
  (println "Part A:" (reduce * (first vs))))
