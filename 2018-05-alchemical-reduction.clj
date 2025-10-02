(defn react [xs]
  (loop [i 0 j 1 rs (transient {})]
    (if (= j (count xs))
      (- (count xs) (* (count rs) 2))
      (if-let [i (rs i)]
        (recur (dec i) j rs)
        (if (< i 0)
          (recur j (inc j) rs)
          (if (= (abs (- (xs i) (xs j))) 32)
            (recur (dec i) (inc j) (assoc! rs j i))
            (recur j (inc j) rs)))))))

(defn process [ch]
  (comp (map int) (remove #{ch (+ ch 32)})))

(defn experiment [xs]
  (comp (map (fn [ch] (into [] (process ch) xs)))
        (map react)))

(let [in (first (line-seq (java.io.BufferedReader. *in*)))]
  (println "Part A:" (react (into [] (map int) in)))
  (println "Part B:" (transduce
                      (experiment in) min ##Inf
                      (range (int \A) (inc (int \Z))))))
