(def mark
  (comp
   (partition-all 2)
   (map (fn [part] (if (reduce = part) 1 0)))))

(defn checksum [xs]
  (let [xs (into [] mark xs)]
    (if (even? (count xs))
      (recur xs)
      (apply str xs))))

(defn step [xs]
  (into (conj xs 0) (map {0 1 1 0}) (rseq xs)))

(defn solve [xs t]
  (if (>= (count xs) t)
    (checksum (subvec xs 0 t))
    (recur (step xs) t)))

(let [in (line-seq (java.io.BufferedReader. *in*))
      xs (into [] (map (comp parse-long str)) (first in))]
  (println "Part A:" (solve xs 272))
  (println "Part B:" (solve xs 35651584)))
