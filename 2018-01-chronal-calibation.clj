(defn rf
  ([] [0 (hash-set)])
  ([r] r)
  ([[r d] x]
   (let [r (+ r x)]
     (if (d r) (reduced r) [r (conj d r)]))))

(let [in (line-seq (java.io.BufferedReader. *in*))
      xf (comp (map (fn [s] (re-seq #"-?\d+" s)))
               (map first)
               (map parse-long))]
  (println "Part A:" (transduce xf + in))
  (println "Part B:" (transduce xf rf (cycle in))))
