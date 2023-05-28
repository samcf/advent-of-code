(let [[ts xs] (line-seq (java.io.BufferedReader. *in*))
      ts (Integer. ts)
      rm (fn [x] [(- x (mod ts x)) x])
      xs (sequence (map #(Integer. %)) (re-seq #"\d+" xs))
      [[rs id] & _] (into (sorted-map) (map rm) xs)]
  (println "Part A:" (* rs id)))
