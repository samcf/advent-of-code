(defn diffs [xs]
  (map (comp (partial apply -) reverse)
       (partition 2 1 xs)))

(defn arrangements [[h & t] a b c]
  (if (seq t)
    (if (= h 1)
      (recur t b c (+ a b c))
      (recur t 0 0 c))
    c))

(let [in (line-seq (java.io.BufferedReader. *in*))
      xs (into (sorted-set) (map #(Integer. %)) in)
      xs (conj xs 0 (+ (apply max xs) 3))]
  (println "Part A:" (->> (diffs xs) (frequencies) (vals) (apply *)))
  (println "Part B:" (->  (diffs xs) (arrangements 0 0 1))))
