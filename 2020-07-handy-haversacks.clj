(def match-key #"(\w+ \w+)")
(def match-val #"(\d) (\w+ \w+) bags?(?:,|.)")

(defn parse [ln]
  (let [[_ color] (re-find match-key ln)
        xf        (comp (map rest) (map (fn [[n c]] [c (Integer. n)])))
        xs        (into {} xf (re-seq match-val ln))]
    [color xs]))

(defn valid? [xs color]
  (or (contains? (xs color) "shiny gold")
      (some (partial valid? xs)
            (keys (xs color)))))

(defn bags [xs color]
  (reduce (fn [s [k v]] (+ s v (* v (bags xs k)))) 0
          (xs color)))

(let [in (line-seq (java.io.BufferedReader. *in*))
      xs (into {} (map parse) in)
      xf (comp (map key)
               (map (partial valid? xs))
               (filter identity))]
  (println "Part A:" (count (sequence xf xs)))
  (println "Part B:" (bags xs "shiny gold")))
