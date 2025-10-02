(defn react [xs]
  (reduce
   (fn [rs x]
     (if (= (abs (- (or (first rs) 0) x)) 32)
       (rest rs)
       (conj rs x))) (list) xs))

(defn process [ch]
  (comp (map int) (remove #{ch (+ ch 32)})))

(defn experiment [xs]
  (comp (map (fn [ch] (into [] (process ch) xs)))
        (map react)
        (map count)))

(let [in (first (line-seq (java.io.BufferedReader. *in*)))]
  (println "Part A:" (count (react (into [] (map int) in))))
  (println "Part B:" (transduce
                      (experiment in) min ##Inf
                      (range (int \A) (inc (int \Z))))))
