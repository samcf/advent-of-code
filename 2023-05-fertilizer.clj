(require '[clojure.string :refer [join]])

(defn src->dst [x [d s r]]
  (when (<= s x (dec (+ s r)))
    (+ d (- x s))))

(defn dst [x rs]
  (or (some (partial src->dst x) rs) x))

(defn location [ys x]
  (reduce dst x ys))

(let [in (line-seq (java.io.BufferedReader. *in*))
      in (sequence (comp (partition-by #{""}) (filter (complement #{[""]}))) in)
      xs (sequence (map parse-long) (re-seq #"\d+" (ffirst in)))
      ys (sequence (comp (map (partial join " "))
                         (map (partial re-seq #"\d+"))
                         (map (partial map parse-long))
                         (map (partial partition 3)))
                   (rest in))]
  (println "Part A:" (transduce (map (partial location ys)) min ##Inf xs)))
