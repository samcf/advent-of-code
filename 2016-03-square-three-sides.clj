(defn triangle? [[a b c]]
  (and (> (+ a b) c)
       (> (+ a c) b)
       (> (+ b c) a)))

(defn counting [pred]
  (keep (fn [x] (when (pred x) 1))))

(let [in (line-seq (java.io.BufferedReader. *in*))
      re (re-seq #"\d+" (apply str in))
      xs (into [] (comp (map parse-long) (partition-all 3)) re)
      ys (into [] (comp cat (partition-all 3)) (apply mapv vector xs))]
  (println "Part A:" (transduce (counting triangle?) + xs))
  (println "Part B:" (transduce (counting triangle?) + ys)))
