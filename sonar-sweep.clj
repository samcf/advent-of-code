(ns advent)

(defn a [coll]
  (->> (partition 2 1 coll)
       (reduce (fn [t [a b]] (+ t (if (< a b) 1 0))) 0)))

(defn b [coll]
  (->> (partition 3 1 coll)
       (partition 2 1)
       (reduce (fn [t [a b]] (+ t (if (< (reduce + a) (reduce + b)) 1 0))) 0)))

(def depths
  (into [] (map #(Integer. %)) (line-seq (java.io.BufferedReader. *in*))))

(println "Part A:" (a depths))
(println "Part B:" (b depths))
