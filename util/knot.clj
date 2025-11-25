(ns knot)

(def salt [17 31 73 47 23])

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

(defn spans [s]
  (take
   (* (+ (count s) (count salt)) 64)
   (cycle (concat (map int s) salt))))

(defn ^:export rf
  ([] {:xs (vec (range 256)) :idx 0 :skp 0})
  ([rs] (:xs rs))
  ([rs len]
   (->
    rs
    (update :xs twist (:idx rs) len)
    (update :idx + (:skp rs) len)
    (update :skp inc))))

(def ^:export encode
  (comp
   (partition-all 16)
   (map (partial reduce bit-xor))
   (mapcat (partial format "%02x"))))

(defn ^:export create [s]
  (transduce (map identity) rf (spans s)))
