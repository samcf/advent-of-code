(ns advent)

(defn a [coll]
  (->> (apply mapv vector coll)
       (map (fn [x] (reduce (fn [t n] (if (= n \1) (inc t) t)) 0 x)))
       (map (fn [x] (if (> x (/ (count coll) 2)) [1 0] [0 1])))
       (apply mapv vector)
       (map (fn [x] (apply str x)))
       (map (fn [x] (Integer/parseInt x 2)))
       (apply *)))

(def reports
  (into [] (line-seq (java.io.BufferedReader. *in*))))

(println "Part A:" (a reports))
