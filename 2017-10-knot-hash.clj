(defn twist [xs src len]
  (let [xf (comp
            (map (fn [idx] (+ src idx)))
            (map (fn [idx] (mod idx (count xs)))))
        rn (range len)
        ix (into [] xf rn)
        rx (into [] (reverse ix))]
    (persistent!
     (reduce
      (fn [rs idx] (assoc! rs (ix idx) (xs (rx idx))))
      (transient xs) rn))))

(defn knot
  ([] {:xs (vec (range 256)) :idx 0 :skp 0})
  ([rs] (reduce * (take 2 (:xs rs))))
  ([rs len]
   (->
    rs
    (update :xs twist (:idx rs) len)
    (update :idx + (:skp rs) len)
    (update :skp inc))))

(defn encode [xs]
  (transduce
   (comp
    (map (partial reduce bit-xor))
    (map (partial format "%02x"))) str
   (partition 16 xs)))

(let [in (first (line-seq (java.io.BufferedReader. *in*)))
      xs (into (into [] (map int) in) [17 31 73 47 23])
      xs (take (* (count xs) 64) (cycle xs))]
  (println "Part A:" (transduce (map parse-long) knot (re-seq #"\d+" in)))
  (println "Part B:" (encode (:xs (reduce knot (knot) xs)))))
