(defn escape [xs f]
  (loop [xs (transient xs) i 0 t 0]
    (let [o (xs i) x (+ i o)]
      (if (< x (count xs))
        (recur (assoc! xs i (f o)) x (inc t))
        (inc t)))))

(let [in (line-seq (java.io.BufferedReader. *in*))
      xs (into [] (map parse-long) in)]
  (println "Part A:" (escape xs inc))
  (println "Part B:" (escape xs (fn [o] (if (> o 2) (dec o) (inc o))))))
