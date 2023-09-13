(require '[clojure.string :refer [replace]])

(defn parse [s]
  (condp re-find s
    #"mem\[(\d+)\] = (\d+)" :>> (fn [[_ x y]] [(Integer. x) (Integer. y)])
    #"mask = (\w+)"         :>> (fn [[_ x]] x)))

(defn solve-a
  ([r] (transduce (map val) + r))
  ([r [s & xs]]
   (let [o (Long/parseLong (replace s #"X" "0") 2)
         z (Long/parseLong (replace s #"[X10]" {"X" "0" "1" "0" "0" "1"}) 2)
         f (fn [[_ v]] (- (bit-or v o) (bit-and v z)))]
     (into r (map (juxt first f)) xs))))

(let [in (line-seq (java.io.BufferedReader. *in*))
      xf (comp (map parse)
               (partition-by string?)
               (partition-all 2)
               (map (partial apply into)))]
  (time (println "Part A:" (transduce xf solve-a {} in))))
