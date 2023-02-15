(defn diffs [xs]
  (mapv (comp (partial apply -) reverse)
        (partition 2 1 xs)))

(defn arrangements [[h & t] a b c]
  (if (seq t)
    (if (= h 1)
      (recur t b c (+ a b c))
      (recur t 0 0 c))
    c))

(let [in (line-seq (java.io.BufferedReader. *in*))
      xs (into (sorted-set 0) (map #(Integer. %)) in)
      xs (conj (diffs xs) 3)]
  (println "Part A:" (reduce * (vals (frequencies xs))))
  (println "Part B:" (arrangements xs 0 0 1)))
