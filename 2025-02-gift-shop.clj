(require '[clojure.math :refer [floor log10 pow]])

(defn digits [x]
  (int (inc (floor (log10 x)))))

(defn pattern? [x n]
  (let [base (pow 10 n)]
    (loop [a x b nil]
      (if (zero? a) true
          (let [c (int (mod a base))]
            (if (and b (not= b c)) false
                (recur (quot a base) c)))))))

(defn invalid [x]
  (let [d (digits x)]
    (if (odd? d) false
        (pattern? x (quot d 2)))))

(defn invalid' [x]
  (let [d (digits x)]
    (loop [n (dec d)]
      (if (zero? n) false
          (if (and (zero? (rem d n)) (pattern? x n)) true
              (recur (dec n)))))))

(let [in (first (line-seq (java.io.BufferedReader. *in*)))
      re (re-seq #"\d+" in)
      xf (fn [f]
           (comp
            (map parse-long)
            (partition-all 2)
            (mapcat (fn [[a b]] (range a (inc b))))
            (keep (fn [x] (when (f x) x)))))]
  (println "Part A:" (transduce (xf invalid)  + re))
  (println "Part B:" (transduce (xf invalid') + re)))
