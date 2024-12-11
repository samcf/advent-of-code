(require '[clojure.math :refer [log10 pow]])

(def stones
  (memoize
   (fn [x itr max]
     (if (< itr max)
       (if (= x 0)
         (recur 1 (inc itr) max)
         (let [d (int (inc (log10 x)))]
           (if (even? d)
             (let [div (pow 10 (/ d 2))]
               (+ (stones (int (quot x div)) (inc itr) max)
                  (stones (int (mod  x div)) (inc itr) max)))
             (recur (* x 2024) (inc itr) max)))) 1))))

(let [in (re-seq #"\d+" (first (line-seq (java.io.BufferedReader. *in*))))
      xf (fn [max]
           (comp (map str)
                 (map parse-long)
                 (map (fn [x] (stones x 0 max)))))]
  (println "Part A:" (transduce (xf 25) + in))
  (println "Part B:" (transduce (xf 75) + in)))
