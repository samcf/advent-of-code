(defn parse [ln]
  (condp re-find ln
    #"rect (\d+)x(\d+)"           :>> (fn [[_ a b]] ['rct (parse-long a) (parse-long b)])
    #"rotate row.*?(\d+).*?(\d+)" :>> (fn [[_ a b]] ['row (parse-long a) (parse-long b)])
    #"rotate col.*?(\d+).*?(\d+)" :>> (fn [[_ a b]] ['col (parse-long a) (parse-long b)])))

(def init
  {50 (vec (repeat 50 0))
   6  (vec (repeat  6 0))})

(def indices
  (comp cat (keep-indexed (fn [idx val] (when (= val 1) idx)))))

(defn transpose [xs]
  (apply mapv vector xs))

(defn fill [xs dx dy]
  (reduce-kv
   (fn [xs y row]
     (if (< y dy)
       (->>
        (range dx)
        (mapcat (fn [idx] [idx 1]))
        (apply assoc row)
        (assoc xs y)) xs)) xs xs))

(defn rotate [xs n]
  (reduce-kv
   (fn [xs idx val]
     (if (= val 1)
       (assoc xs (mod (+ idx n) (count xs)) 1)
       xs)) (init (count xs)) xs))

(defn solve
  ([] (into [] (repeat 6 (init 50))))
  ([xs] (into (hash-set) indices xs))
  ([xs [ins a b]]
   (case ins
     rct (fill xs a b)
     row (update xs a rotate b)
     col (transpose (update (transpose xs) a rotate b)))))

(let [in (line-seq (java.io.BufferedReader. *in*))
      xs (transduce (map parse) solve in)]
  (println "Part A:" (count xs))
  (println "Part B:")
  (doseq [y (range 6)]
    (println
     (transduce
      (comp
       (map (fn [x] (+ (* y 50) x)))
       (map (fn [i] (if (xs i) \█ \░)))) str
      (range 50)))))
