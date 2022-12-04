(require '[clojure.string :refer [split]])

(defn superset? [[a b c d]]
  (or (and (<= a c) (>= b d))
      (and (<= c a) (>= d b))))

(defn subset? [[a b c d]]
  (or (and (>= a c) (<= a d))
      (and (>= b c) (<= b d))
      (and (>= c a) (<= c b))
      (and (>= d a) (<= d b))))

(let [lns (line-seq (java.io.BufferedReader. *in*))
      xfr (comp (map (fn [ln] (split ln #","))) cat
                (map (fn [pr] (split pr #"-"))) cat
                (map (fn [nm] (Integer. nm)))
                (partition-all 4))]
  (println "Part A:" (transduce (comp xfr (map superset?) (map #(if % 1 0))) + lns))
  (println "Part B:" (transduce (comp xfr (map subset?) (map #(if % 1 0))) + lns)))
