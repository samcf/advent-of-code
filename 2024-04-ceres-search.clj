(def cards [[0 -1] [1  0] [0 1] [-1 0]])
(def diags [[-1 -1] [1 -1] [-1 1] [1 1]])
(def steps (list 1 2 3))
(def cross #{-990226162 1589543312 -411337452 1188667126})

(defn offsets [pairs len]
  (map (fn [[x y]] (+ x (* y len))) pairs))

(defn matches-a [xs idx offsets]
  (reduce + (for [offset offsets
                  step   steps
                  :let   [val (get xs (+ idx (* step offset)))]
                  :while (case step 1 (= val \M) 2 (= val \A) 3 (= val \S))
                  :when  (= step 3)]
              1)))

(defn matches-b [xs idx offsets]
  (if (cross (hash (map (fn [off] (get xs (+ idx off))) offsets))) 1 0))

(let [in (line-seq (java.io.BufferedReader. *in*))
      ln (inc (count (first in)))
      xs (into [] (mapcat (fn [s] (str s \.))) in)
      rs (range (count xs))
      xf (fn [f char offsets]
           (comp (filter (fn [idx] (= (get xs idx) char)))
                 (map    (fn [idx] (f xs idx offsets)))))]
  (println "Part A:" (transduce (xf matches-a \X (offsets (into cards diags) ln)) + rs))
  (println "Part B:" (transduce (xf matches-b \A (offsets diags ln)) + rs)))
