(defn ordered-fn? [rules]
  (fn [xs]
    (loop [xs xs after (into #{} (rest xs))]
      (if (seq xs)
        (let [[x & xs] xs]
          (if (every? (rules x #{}) after)
            (recur xs (disj after (first xs))) false)) true))))

(defn compare-fn [rules]
  (fn [x y]
    (if (get (rules x) y) -1 1)))

(defn mid [xs]
  (nth xs (/ (count xs) 2)))

(let [lines (line-seq (java.io.BufferedReader. *in*))
      parse (comp (map (fn [ln] (re-seq #"\d+" ln)))
                  (map (fn [xs] (map parse-long xs)))
                  (partition-by (complement seq))
                  (filter (comp seq first)))
      parts (into [] parse lines)
      rules (reduce (fn [m [x y]]
                      (if (m x)
                        (update m x conj y)
                        (update m x hash-set y))) {} (first parts))
      updts (map (juxt (ordered-fn? rules) identity) (second parts))
      xform (fn [f m] (comp (filter f) (map second) (map m) (map mid)))]
  (println "Part A:" (transduce (xform first identity) + updts))
  (println "Part B:" (transduce (xform (complement first)
                                       (partial sort (compare-fn rules)))
                                + updts)))
