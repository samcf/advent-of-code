(require '[clojure.math :refer [floor log10 pow sqrt]])

(def factors
  (memoize
   (fn [n]
     (sequence
      (comp
       (filter (fn [x] (zero? (mod n x))))
       (mapcat (fn [x] [x (quot n x)]))
       (distinct)
       (remove #{n}))
      (range 1 (long (inc (sqrt n))))))))

(defn digits [x]
  (long (inc (floor (log10 x)))))

(defn pattern? [x n]
  (let [base (pow 10 n)]
    (loop [a x b nil]
      (if (zero? a) true
          (let [c (long (mod a base))]
            (if (and b (not= b c)) false
                (recur (quot a base) c)))))))

(defn invalid [x]
  (let [d (digits x)]
    (if (odd? d) false
        (pattern? x (quot d 2)))))

(defn invalid' [x]
  (some
   (fn [n] (when (pattern? x n) x))
   (factors (digits x))))

(let [in (first (line-seq (java.io.BufferedReader. *in*)))
      re (re-seq #"\d+" in)
      xf (comp
          (map parse-long)
          (partition-all 2)
          (mapcat (fn [[a b]] (range a (inc b)))))]
  (println "Part A:" (transduce (comp xf (filter invalid))  + re))
  (println "Part B:" (transduce (comp xf (filter invalid')) + re)))
