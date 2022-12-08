(defn paths [len idx]
  [(range (inc idx) (+ idx (- len (mod idx len))))
   (range (+ idx len) (* len len) len)
   (reverse (range (mod idx len) idx len))
   (reverse (range (- idx (mod idx len)) idx))])

(defn xf-paths [len]
  (map (juxt identity (partial paths len))))

(defn xf-heights [xs]
  (map (fn [[idx paths]] (into [(xs idx)] (map #(map xs %) paths)))))

(def xf-score-a
  (map (fn [[x & xs]]
         (if (some (fn [xs] (every? #(> x %) xs)) xs)
           1 0))))

(def xf-score-b
  (map (fn [[x & xs]]
         (->> (map #(reduce (fn [s n] (if (<= x n) (reduced (inc s)) (inc s))) 0 %) xs)
              (apply *)))))

(let [lns (line-seq (java.io.BufferedReader. *in*))
      len (count (first lns))
      grd (into [] (comp cat (map str) (map #(Integer. %))) lns)
      rng (range (count grd))
      xfs (comp (xf-paths len) (xf-heights grd))]
  (println "Part A:" (transduce (comp xfs xf-score-a) + rng))
  (println "Part B:" (apply max (into [] (comp xfs xf-score-b) rng))))
