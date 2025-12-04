(defn parse [xs]
  (reduce-kv
   (fn [s k v] (if (= v \@) (conj s k) s))
   (hash-set) xs))

(defn surrounding [idx len]
  (let [col (mod idx len)
        lft (> col 0)
        rgt (< col (dec len))
        top (> idx (dec len))
        bot (< idx (- (* len len) len))]
    (cond-> []
      lft (conj (dec idx))
      rgt (conj (inc idx))
      top (conj (- idx len))
      bot (conj (+ idx len))
      (and lft top) (conj (- idx len  1))
      (and rgt top) (conj (- idx len -1))
      (and lft bot) (conj (+ idx len -1))
      (and rgt bot) (conj (+ idx len  1)))))

(defn neighbors [xs idx]
  (sequence (filter xs) (surrounding idx 135)))

(defn removable [xs]
  (loop [rem xs act xs]
    (if-let [idx (first rem)]
      (let [idxs (neighbors act idx)]
        (if (< (count idxs) 4)
          (recur (into (disj rem idx) idxs) (disj act idx))
          (recur (disj rem idx) act)))
      (- (count xs) (count act)))))

(let [in (line-seq (java.io.BufferedReader. *in*))
      xs (parse (into [] cat in))
      xf (comp
          (map (partial neighbors xs))
          (map count)
          (filter #{0 1 2 3})
          (map (constantly 1)))]
  (println "Part A:" (transduce xf + xs))
  (println "Part B:" (removable xs)))
