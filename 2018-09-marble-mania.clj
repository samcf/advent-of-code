(import '[java.util ArrayDeque])

(defn rotate! [xs n]
  (if (pos? n)
    (dotimes [_ n]       (.addFirst xs (.removeLast  xs)))
    (dotimes [_ (abs n)] (.addLast  xs (.removeFirst xs)))))

(defn score [n t]
  (let [xs (ArrayDeque.)]
    (.addFirst xs 0)
    (loop [x 1 s {}]
      (let [p (mod x n)]
        (if (> x t)
          (val (apply max-key val s))
          (if (zero? (mod x 23))
            (do (rotate! xs -6)
                (recur (inc x) (update s p (fnil + 0) (.pop xs) x)))
            (do (rotate! xs 2)
                (.addLast xs x)
                (recur (inc x) s))))))))

(let [in (first (line-seq (java.io.BufferedReader. *in*)))
      [players limit] (into [] (map parse-long) (re-seq #"\d+" in))]
  (println "Part A:" (score players limit))
  (println "Part B:" (score players (* limit 100))))
