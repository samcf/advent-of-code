(def parse-a
  (fn [t]
    (comp (filter (fn [x] (not= x "x")))
          (map (fn [x] (Integer. x)))
          (map (juxt (fn [x] (- x (mod t x))) identity)))))

(def parse-b
  (comp (map-indexed vector)
        (filter (fn [[_ x]] (not= x "x")))
        (map (fn [[i x]] [i (Integer. x)]))))

(defn repeats-at
  ([[t _]] t)
  ([[t a] [o b]]
   (loop [x (+ t a) y 0]
     (case [(zero? (mod (+ x o) b)) (pos? y)]
       [false false] (recur (+ x a) 0)
       [false  true] (recur (+ x a) y)
       [true  false] (recur (+ x a) x)
       [true   true] [y (- x y)]))))

(let [[t b] (line-seq (java.io.BufferedReader. *in*))]
  (println "Part A:" (->> (re-seq #"\d+|x" b) (into (sorted-map) (parse-a (Integer. t))) (first) (reduce *)))
  (println "Part B:" (->> (re-seq #"\d+|x" b) (transduce parse-b repeats-at [0 1]))))
