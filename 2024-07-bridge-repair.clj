(require '[clojure.math :refer [log10 pow]])

(defn join [x y] (long (+ (* x (pow 10 (int (inc (log10 y))))) y)))

(defn solve-a [t x [y & xs]]
  (if (> x t) false
      (let [a (+ x y) b (* x y)]
        (if (seq xs)
          (or (solve-a t a xs) (solve-a t b xs))
          (or (= t a) (= t b))))))

(defn solve-b [t x [y & xs]]
  (if (> x t) false
      (let [a (+ x y) b (* x y) c (join x y)]
        (if (seq xs)
          (or (solve-b t a xs) (solve-b t b xs) (solve-b t c xs))
          (or (= t a) (= t b) (= t c))))))

(let [in (line-seq (java.io.BufferedReader. *in*))
      xf (fn [f]
           (comp (map (fn [ln] (re-seq #"\d+" ln)))
                 (map (fn [xs] (map parse-long xs)))
                 (filter (fn [[target & xs]] (f target 0 xs)))
                 (map first)))]
  (println "Part A:" (transduce (xf solve-a) + in))
  (println "Part B:" (transduce (xf solve-b) + in)))
