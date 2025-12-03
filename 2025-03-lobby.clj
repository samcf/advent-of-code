(defn max-kv [xs]
  (reduce-kv
   (fn [r k v]
     (let [[_ b] r]
       (if (> v b) [k v] r))) [0 0] xs))

(defn joltage [xs n]
  (loop [idx 0 res []]
    (if (= (count res) n)
      (reduce (fn [x d] (+ (* x 10) d)) 0 res)
      (let [xs (subvec xs idx (+ (- (count xs) n) (count res) 1))
            kv (max-kv xs)]
        (recur (+ idx (inc (first kv)))
               (conj res (second kv)))))))

(let [in (line-seq (java.io.BufferedReader. *in*))
      xf (fn [n]
           (comp
            (map (fn [ln] (re-seq #"\d" ln)))
            (map (fn [xs] (into [] (map parse-long) xs)))
            (map (fn [xs] (joltage xs n)))))]
  (println "Part A:" (transduce (xf 2)  + in))
  (println "Part B:" (transduce (xf 12) + in)))
