(require '[clojure.string :refer [split triml]]
         '[clojure.set :refer [intersection]])

(def masks [0x1F00000 0xF8000 0x7C00 0x3E0 0x1F 0x1084210 0x842108 0x421084 0x210842 0x108421])

(defn play [xs board]
  (loop [prev 0 [x & xs] xs]
    (if-let [index (board x)]
      (let [next (bit-or prev (bit-shift-left 1 (- 24 index)))]
        (if (some #(= (bit-and next %) %) masks) [(keys board) xs x] (recur next xs)))
      (recur prev xs))))

(defn score-kv [[ks xs x]]
  [(count xs) (* (reduce + (intersection (set xs) (set ks))) x)])

(let [lines (line-seq (java.io.BufferedReader. *in*))
      drawn (map #(Integer. %) (split (first lines) #","))
      score (into (sorted-map)
                  (comp (filter seq)
                        (map triml)
                        (map (fn [line] (split line #"\s+")))
                        (partition-all 5)
                        (map flatten)
                        (map (fn [xs] (map #(Integer. %) xs)))
                        (map (fn [xs] (map-indexed (comp vec rseq vector) xs)))
                        (map (fn [xs] (into {} xs)))
                        (map (fn [board] (play drawn board)))
                        (map score-kv)) (rest lines))]
  (println "Part A:" (val (last score)))
  (println "Part B:" (val (first score))))
