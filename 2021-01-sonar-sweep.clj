(defn a [coll]
  (reduce (fn [t [a b]] (+ t (if (< a b) 1 0))) 0
          (partition 2 1 coll)))

(defn b [coll]
  (reduce (fn [t [a b]] (+ t (if (< (reduce + a) (reduce + b)) 1 0))) 0
          (->> coll (partition 3 1) (partition 2 1))))

(def depths
  (into [] (map #(Integer. %)) (line-seq (java.io.BufferedReader. *in*))))

(println "Part A:" (a depths))
(println "Part B:" (b depths))
