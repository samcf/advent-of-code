(defn paths [idx len]
  (list (reverse (range (mod idx len) idx len))
        (range (inc idx) (+ idx (- len (mod idx len))))
        (range (+ idx len) (* len len) len)
        (reverse (range (- idx (mod idx len)) idx))))

(defn score-a [xs len]
  (fn [[idx val]]
    (if (some (fn [idxs] (every? #(> val (second (xs %))) idxs))
              (paths idx len)) 1 0)))

(defn score-b [xs len]
  (fn [[idx hgt]] (->> (paths idx len)
                       (map #(reduce (fn [sum idx]
                                       (if (<= hgt (second (xs idx)))
                                         (reduced (inc sum))
                                         (inc sum))) 0 %))
                       (apply *))))

(def xf-line
  (comp cat
        (map str)
        (map #(Integer. %))
        (map-indexed vector)))

(let [lns (line-seq (java.io.BufferedReader. *in*))
      len (count (first lns))
      xs  (into [] xf-line lns)]
  (println "Part A:" (transduce (map (score-a xs len)) + xs))
  (println "Part B:" (apply max (map (score-b xs len) xs))))
