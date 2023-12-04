(require '[clojure.set :refer [intersection]]
         '[clojure.math :refer [pow]])

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
  (println "Part A:" (transduce (comp xf (comp (map dec) (map (partial pow 2)) (map int))) + in))
  (println "Part B:" (transduce xf totals-rf '() in)))
