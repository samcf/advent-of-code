(defn run [xs]
  (loop [idx 0 xs (transient xs)]
    (let [a (xs (+ idx 1))
          b (xs (+ idx 2))
          c (xs (+ idx 3))]
      (case (xs idx)
        1 (recur (+ idx 4) (assoc! xs c (+ (xs a) (xs b))))
        2 (recur (+ idx 4) (assoc! xs c (* (xs a) (xs b))))
        99 (xs 0)))))

(defn match [xs target]
  (let [r (range (dec (count xs)))]
    (for [a r b r :when (= (run (assoc xs 1 a 2 b)) target)]
      (+ (* 100 a) b))))

(let [in (first (line-seq (java.io.BufferedReader. *in*)))
      xs (into [] (map parse-long) (re-seq #"\d+" in))]
  (println "Part A:" (run (assoc xs 1 12 2 2)))
  (println "Part B:" (first (match xs 19690720))))
