(def match-key #"(\w+ \w+)")
(def match-val #"(\d) (\w+ \w+) bags?(?:,|.)")

(defn parse [ln]
  (let [[_ color] (re-find match-key ln)]
    (-> (comp (map rest) (map (fn [[n c]] [c (Integer. n)])) cat)
        (sequence (re-seq match-val ln))
        (conj color))))

(defn group [[color & rest]]
  [color (apply hash-map rest)])

(defn valid? [xs color]
  (or (contains? (xs color) "shiny gold")
      (some (partial valid? xs)
            (keys (xs color)))))

(defn bags [xs color]
  (reduce (fn [s [k v]] (+ s v (* v (bags xs k)))) 0
          (xs color)))

(let [in (line-seq (java.io.BufferedReader. *in*))
      xf (comp (map parse) (map group))
      xs (into {} xf in)
      xv (comp (map key) (map (partial valid? xs)) (filter identity))]
  (println "Part A:" (count (sequence xv xs)))
  (println "Part B:" (bags xs "shiny gold")))
