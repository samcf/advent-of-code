(defn largest [xs]
  (reduce-kv
   (fn [idx key val]
     (if (> val (xs idx)) key idx)) 0 xs))

(defn indices [src len num]
  (eduction
   (map (fn [idx] (+ src idx)))
   (map (fn [idx] (mod idx len)))
   (range 1 (inc num))))

(defn redistribute [xs]
  (let [idx (largest xs)]
    (reduce
     (fn [xs idx] (update xs idx inc))
     (assoc xs idx 0)
     (indices idx (count xs) (xs idx)))))

(defn solve [xs]
  (loop [xs xs vs (hash-set) cycles 0]
    (if (vs xs) [cycles xs]
        (recur (redistribute xs) (conj vs xs) (inc cycles)))))

(let [in (first (line-seq (java.io.BufferedReader. *in*)))
      xs (into [] (map parse-long) (re-seq #"\d+" in))
      rs (solve xs)]
  (println "Part A:" (first rs))
  (println "Part B:" (first (solve (second rs)))))
