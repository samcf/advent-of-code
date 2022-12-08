(defn paths
  ([idx] (paths idx 99))
  ([idx len]
   [(range (+ idx len) (* len len) len)
    (range (- idx len) (- (mod idx len) len) (- len))
    (range (inc idx) (+ idx (- len (mod idx len))))
    (range (dec idx) (dec (- idx (mod idx len))) -1)]))

(def xf-score-a
  (map (fn [[x & xs]]
         (if (some (fn [xs] (every? #(> x %) xs)) xs)
           1 0))))

(def xf-score-b
  (map (fn [[x & xs]]
         (transduce (map #(reduce (fn [s n] (if (<= x n) (reduced (inc s)) (inc s))) 0 %))
                    * xs))))

(let [lns (line-seq (java.io.BufferedReader. *in*))
      grd (into [] (comp cat (map int)) lns)
      rng (range (count grd))
      xfs (comp (map (juxt identity paths))
                (map (fn [[idx paths]]
                       (conj (map #(map grd %) paths)
                             (grd idx)))))]
  (println "Part A:" (transduce (comp xfs xf-score-a) + rng))
  (println "Part B:" (first (into (sorted-set-by >) (comp xfs xf-score-b) rng))))
