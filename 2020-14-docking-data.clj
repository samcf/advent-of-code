(require '[clojure.string :refer [replace]])

(defn parse-assign [s]
  (let [[_ a v] (re-find #"mem\[(\d+)\] = (\d+)" s)]
    [(Integer. a) (Integer. v)]))

(defn parse [[[mask] xs]]
  (let [o-mask (Long/parseLong (replace (subs mask 7) #"X"  {"X" "0"}) 2)
        z-mask (Long/parseLong (replace (subs mask 7) #"\w" {"X" "0" "1" "0" "0" "1"}) 2)]
    (into [o-mask z-mask] (map parse-assign) xs)))

(defn solve-a
  ([r] (transduce (map val) + r))
  ([r [o z & xs]]
   (let [f (fn [[_ v]] (- (bit-or v o) (bit-and v z)))]
     (into r (map (juxt first f)) xs))))

(let [in (line-seq (java.io.BufferedReader. *in*))
      xf (comp (partition-by (fn [s] (= (subs s 0 4) "mask")))
               (partition-all 2)
               (map parse))]
  (println "Part A:" (transduce xf solve-a {} in)))
