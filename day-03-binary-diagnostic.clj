(ns advent)

(defn a [coll]
  (->> (apply mapv vector coll)
       (map (fn [x] (reduce (fn [t n] (if (= n \1) (inc t) t)) 0 x)))
       (map (fn [x] (if (> x (/ (count coll) 2)) [1 0] [0 1])))
       (apply mapv vector)
       (map (fn [x] (apply str x)))
       (map (fn [x] (Integer/parseInt x 2)))
       (apply *)))

(defn f [xs idx hi lo]
  (if (seq (rest xs))
    (let [col (map (fn [x] (nth x idx)) xs)
          sum (reduce (fn [t x] (if (= x \1) (inc t) t)) 0 col)
          res (if (>= sum (/ (count xs) 2)) hi lo)]
      (recur (filter (fn [x] (= (get x idx) res)) xs) (inc idx) hi lo))
    (Integer/parseInt (apply str (first xs)) 2)))

(defn b [coll]
  (* (f coll 0 \1 \0) (f coll 0 \0 \1)))

(def reports
  (into [] (line-seq (java.io.BufferedReader. *in*))))

(println "Part A:" (a reports))
(println "Part B:" (b reports))
