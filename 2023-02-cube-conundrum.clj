(require '[clojure.string :refer [split]])

(def re-game #"Game (\d+): (.*)")
(def re-hand #"(\d+) (red|green|blue)")

(defn parse-hand [ln]
  (sequence (map (fn [[_ n c]] (list (keyword c) (parse-long n))))
            (re-seq re-hand ln)))

(defn totals [pairs]
  (reduce (fn [counts [color count]]
            (update counts color (partial + count)))
          {:red 0 :blue 0 :green 0}
          pairs))

(defn parse [ln]
  (let [[[_ id ln]] (re-seq re-game ln)]
    (list (parse-long id)
          (sequence (comp (map parse-hand) (map totals))
                    (split ln #"; ")))))

(defn valid? [hand]
  (and (<= (get hand :red   0) 12)
       (<= (get hand :blue  0) 14)
       (<= (get hand :green 0) 13)))

(defn power [hands]
  (->> (map (juxt :red :blue :green) hands)
       (apply mapv vector)
       (map (partial apply max))
       (apply *)))

(let [in (line-seq (java.io.BufferedReader. *in*))]
  (println "Part A:" (transduce (comp (map parse) (filter (comp (partial every? valid?) second)) (map first)) + in))
  (println "Part B:" (transduce (comp (map parse) (map (comp power second))) + in)))
