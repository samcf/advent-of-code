(require '[clojure.string :refer [split]])

(def re-game #"Game (\d+): (.*)")
(def re-hand #"(\d+) (red|green|blue)")

(defn parse-hand [ln]
  (sequence (map (fn [[_ n c]] {(keyword c) (parse-long n)}))
            (re-seq re-hand ln)))

(defn parse [ln]
  (let [[[_ id ln]] (re-seq re-game ln)]
    [(parse-long id)
     (sequence
      (comp (map parse-hand) (map (partial apply merge-with + {:red 0 :blue 0 :green 0})))
      (split ln #"; "))]))

(defn valid? [hand]
  (and (<= (:red   hand) 12)
       (<= (:blue  hand) 14)
       (<= (:green hand) 13)))

(def sum-ids-xf
  (comp (map parse)
        (filter (comp (partial every? valid?) second))
        (map first)))

(def sum-powers-xf
  (comp (map parse)
        (map second)
        (map (partial apply merge-with max))
        (map vals)
        (map (partial reduce *))))

(let [in (line-seq (java.io.BufferedReader. *in*))]
  (println "Part A:" (transduce sum-ids-xf + in))
  (println "Part B:" (transduce sum-powers-xf + in)))
