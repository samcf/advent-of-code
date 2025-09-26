(defn join [xs]
  (reduce (fn [x d] (+ (* x 10) d)) 0 xs))

(defn element [xs n]
  (loop [idx (dec n) rs 0]
    (if (> idx (dec (count xs)))
      (mod (abs rs) 10)
      (case (mod (quot (inc idx) n) 4)
        1 (recur (inc idx) (+ rs (xs idx)))
        3 (recur (inc idx) (+ rs (* (xs idx) -1)))
        (recur (+ idx n) rs)))))

(defn phase [xs]
  (loop [n 1 rs []]
    (if (> n (count xs)) rs
        (recur (inc n) (conj rs (element xs n))))))

(let [in (first (line-seq (java.io.BufferedReader. *in*)))
      xs (into [] (map parse-long) (re-seq #"\d" in))]
  (println "Part A:" (join (take 8 (nth (iterate phase xs) 100)))))
