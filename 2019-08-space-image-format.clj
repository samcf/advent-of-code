(def color (map (fn [x] (if (= x 0) \░ \█))))
(def zs (range 100))
(def xs 25)
(def ys 6)

(defn decode [vs]
  (for [y (range ys) x (range xs)]
    (first
     (sequence
      (comp
       (map (fn [z] (+ (* xs y) (* xs ys z) x)))
       (map vs)
       (filter #{0 1})) zs))))

(let [in (first (line-seq (java.io.BufferedReader. *in*)))
      vs (into [] (map parse-long) (re-seq #"\d" in))
      xf (comp (partition-all (* xs ys)) (map frequencies))
      fq (first (sort-by (fn [fq] (fq 0)) < (sequence xf vs)))]
  (println "Part A:" (* (fq 1) (fq 2)))
  (println "Part B:")
  (doseq [row (partition xs (decode vs))]
    (println (transduce color str row))))
