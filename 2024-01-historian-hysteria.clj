(let [in (line-seq (java.io.BufferedReader. *in*))
      xf (fn [f]
           (comp (map (fn [ln] (re-seq #"\d+" ln)))
                 (map f)
                 (map parse-long)))
      xs (sort (into [] (xf first)  in))
      ys (sort (into [] (xf second) in))
      fq (frequencies ys)]
  (println "Part A:" (reduce +  (map (comp abs -) xs ys)))
  (println "Part B:" (transduce (map (fn [x] (* (fq x 0) x))) + xs)))
