(defn select [f]
  (map (fn [xs] (key (apply f val (frequencies xs))))))

(let [in (line-seq (java.io.BufferedReader. *in*))
      xs (apply mapv vector in)]
  (println "Part A:" (transduce (select max-key) str xs))
  (println "Part B:" (transduce (select min-key) str xs)))
