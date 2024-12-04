(def cards [[0 -1] [1  0] [0 1] [-1 0]])
(def diags [[-1 -1] [1 -1] [-1 1] [1 1]])

(defn offsets [pairs len]
  (map (fn [[x y]] (+ x (* y len))) pairs))

(defn matches-a [xs idx offsets]
  (reduce + (for [offset      offsets
                  [step test] [[1 \M] [2 \A] [3 \S]]
                  :let        [next (get xs (+ idx (* step offset)))]
                  :while      (= next test)
                  :when       (= step 3)]
              1)))

(defn matches-b [xs idx offsets]
  (let [xf (comp (map (fn [x] (+ idx x))) (map (fn [i] (get xs i))))
        xs (into [] xf offsets)]
    (if (#{"SSMM" "SMSM" "MMSS" "MSMS"} (apply str xs))
      1 0)))

(let [in (line-seq (java.io.BufferedReader. *in*))
      ln (inc (count (first in)))
      xs (into [] (mapcat (fn [s] (str s \.))) in)
      rs (range (count xs))
      xf (fn [f char offsets]
           (comp (filter (fn [idx] (= (get xs idx) char)))
                 (map    (fn [idx] (f xs idx offsets)))))]
  (println "Part A:" (transduce (xf matches-a \X (offsets (into cards diags) ln)) + rs))
  (println "Part B:" (transduce (xf matches-b \A (offsets diags ln)) + rs)))
