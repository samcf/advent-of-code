(defn disjoint [a b]
  (let [[ax ay] (peek a) [bx by] b]
    (if (<= (dec (max ax bx)) (min ay by))
      (conj (pop a) [(min ax bx) (max ay by)])
      (conj a b))))

(let [in (line-seq (java.io.BufferedReader. *in*))
      xf (comp (map (fn [ln] (re-seq #"\d+" ln)))
               (map (fn [xs] (into [] (map parse-long) xs))))
      xs (sort (into [] xf in))
      xs (reduce disjoint [(first xs)] (rest xs))]
  (println "Part A:" (inc (second (first xs))))
  (println "Part B:" (dec (count xs))))
