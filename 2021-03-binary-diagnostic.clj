(defn transpose [xs]
  (apply mapv vector xs))

(defn decimal [xs]
  (reduce (fn [r x] (+ (* 2 r) x)) 0 xs))

(defn majority [xs]
  (let [fq (frequencies xs)]
    (if (apply = (vals fq)) 1
        (ffirst (sort-by val > fq)))))

(defn solve-a
  ([xs] (solve-a xs identity))
  ([xs rule-fn]
   (let [xf (comp (map majority) (map rule-fn))]
     (->> (transpose xs) (sequence xf) (decimal)))))

(defn solve-b
  ([xs] (solve-b xs identity))
  ([xs rule-fn]
   (loop [xs xs idx 0]
     (if (> (count xs) 1)
       (let [mv (rule-fn (majority (nth (transpose xs) idx)))
             xf (filter (fn [ys] (= (ys idx) mv)))]
         (recur (sequence xf xs) (inc idx)))
       (decimal (first xs))))))

(let [in (line-seq (java.io.BufferedReader. *in*))
      xf (map (partial mapv {\1 1 \0 0}))
      xs (sequence xf in)
      rf {1 0 0 1}]
  (println "Part A:" (* (solve-a xs) (solve-a xs rf)))
  (println "Part B:" (* (solve-b xs) (solve-b xs rf))))
