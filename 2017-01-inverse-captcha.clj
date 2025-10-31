(defn sum [xs step]
  (reduce-kv
   (fn [sum idx val]
     (let [cmp (xs (mod (+ idx step) (count xs)))]
       (if (= val cmp) (+ sum val) sum))) 0 xs))

(let [in (first (line-seq (java.io.BufferedReader. *in*)))
      xs (into [] (map parse-long) (re-seq #"\d" in))]
  (println "Part A:" (sum xs 1))
  (println "Part B:" (sum xs (quot (count xs) 2))))
