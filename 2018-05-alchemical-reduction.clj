(defn react [xs]
  (reduce
   (fn [rs x]
     (if (= (abs (- (or (first rs) 0) x)) 32)
       (rest rs)
       (conj rs x))) (list) xs))

(defn experiment [xs]
  (comp (map (fn [ch] (remove #{ch (+ ch 32)})))
        (map (fn [xf] (into [] xf xs)))
        (map react)
        (map count)))

(let [in (first (line-seq (java.io.BufferedReader. *in*)))
      xs (into [] (map int) in)]
  (println "Part A:" (count (react xs)))
  (println "Part B:" (transduce
                      (experiment xs) min ##Inf
                      (range (int \A) (inc (int \Z))))))
