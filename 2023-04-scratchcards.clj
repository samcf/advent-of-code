(require '[clojure.set :refer [intersection]]
         '[clojure.math :refer [pow]])

(defn points [x]
  (int (pow 2 (dec x))))

(defn totals-rf
  ([xs]   (reduce + xs))
  ([xs x] (conj xs (reduce + 1 (take x xs)))))

(let [in (reverse (line-seq (java.io.BufferedReader. *in*)))
      xf (comp (map (partial re-seq #"\d+"))
               (map rest)
               (map (partial map parse-long))
               (map (partial split-at 10))
               (map (partial map set))
               (map (partial apply intersection))
               (map count))]
  (println "Part A:" (transduce (comp xf (map points)) + in))
  (println "Part B:" (transduce xf totals-rf '() in)))
