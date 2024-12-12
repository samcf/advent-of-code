(require '[clojure.math :refer [log10 pow]])

(def stones
  (memoize
   (fn [x n]
     (if (> n 0)
       (if (= x 0)
         (recur 1 (dec n))
         (let [d (int (inc (log10 x)))]
           (if (even? d)
             (let [div (pow 10 (/ d 2))]
               (+ (stones (int (quot x div)) (dec n))
                  (stones (int (mod  x div)) (dec n))))
             (recur (* x 2024) (dec n))))) 1))))

(let [in (re-seq #"\d+" (first (line-seq (java.io.BufferedReader. *in*))))
      xf (fn [n]
           (comp (map str)
                 (map parse-long)
                 (map (fn [x] (stones x n)))))]
  (println "Part A:" (transduce (xf 25) + in))
  (println "Part B:" (transduce (xf 75) + in)))
