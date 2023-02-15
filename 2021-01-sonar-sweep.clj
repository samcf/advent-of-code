(let [in (line-seq (java.io.BufferedReader. *in*))
      xs (sequence (map #(Integer. %)) in)
      xf (map (fn [[a b]] (if (< a b) 1 0)))
      pf (map (partial map (partial reduce +)))]
  (println "Part A:" (transduce xf + (partition 2 1 xs)))
  (println "Part B:" (transduce (comp pf xf) +
                                (->> (partition 3 1 xs)
                                     (partition 2 1)))))
