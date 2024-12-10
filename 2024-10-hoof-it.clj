(def dirs [[0 -1] [1 0] [0 1] [-1 0]])

(def parse
  (comp (map (fn [ln] (re-seq #"\d" ln)))
        (map (fn [xs] (map parse-long xs)))
        (map vec)
        (map (fn [xs] (conj xs -1)))
        cat))

(def zeros
  (comp (map-indexed vector)
        (filter (comp zero? second))
        (map first)))

(defn trails [xs idx sz]
  (let [val (get xs idx)]
    (if (= val 9) [idx]
        (into [] (comp (map    (fn [[x y]] (+ x (* y sz))))
                       (map    (fn [off] (+ idx off)))
                       (filter (fn [idx] (= (get xs idx) (inc val))))
                       (mapcat (fn [idx] (trails xs idx sz)))) dirs))))

(let [in (line-seq (java.io.BufferedReader. *in*))
      sz (inc (count (first in)))
      xs (into [] parse in)
      zs (into [] zeros xs)
      xf (fn [f] (map (comp count f (fn [idx] (trails xs idx sz)))))]
  (println "Part A:" (transduce (xf distinct) + zs))
  (println "Part B:" (transduce (xf identity) + zs)))
