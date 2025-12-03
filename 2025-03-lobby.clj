(defn max-kv [xs]
  (reduce-kv
   (fn [r k v]
     (let [[_ b] r]
       (if (> v b) [k v] r))) [0 0] xs))

(defn joltage [xs n]
  (loop [idx 0 res 0 rem n]
    (if (zero? rem) res
        (let [[k v] (max-kv (subvec xs idx (inc (- (count xs) rem))))]
          (recur
           (+ idx (inc k))
           (+ (* res 10) v)
           (dec rem))))))

(let [in (line-seq (java.io.BufferedReader. *in*))
      xf (fn [n]
           (comp
            (map (fn [ln] (re-seq #"\d" ln)))
            (map (fn [xs] (into [] (map parse-long) xs)))
            (map (fn [xs] (joltage xs n)))))]
  (println "Part A:" (transduce (xf 2)  + in))
  (println "Part B:" (transduce (xf 12) + in)))
