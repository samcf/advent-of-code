(def parse-xf
  (comp (map (fn [ln] (re-seq #"\d+" ln)))
        (map (fn [xs] (map parse-long xs)))
        (partition-by (complement seq))
        (filter (comp seq first))))

(defn ordered-fn? [rules]
  (fn [xs]
    (if (seq xs)
      (let [[x & xs] xs]
        (if (every? (rules x #{}) xs)
          (recur xs) false)) true)))

(defn compare-fn [rules]
  (fn [x y]
    (if (get (rules x) y) -1 1)))

(defn mid [xs]
  (nth xs (/ (count xs) 2)))

(let [in (line-seq (java.io.BufferedReader. *in*))

      ;; parse the input into two parts, the rules and the
      ;; update lists
      rs (into [] parse-xf in)

      ;; create a map of rules whose keys are page numbers
      ;; and who values are sets of pages that must appear
      ;; after
      ru (reduce (fn [m [x y]] (update m x (fnil conj #{}) y)) {} (first rs))

      ;; describe each update list as either ordered or not
      xs (map (juxt (ordered-fn? ru) identity) (second rs))

      ;; pick out the appropriate update list, do any
      ;; necessary transforms on it, and return the middle
      ;; element
      xf (fn [f m] (comp (filter f) (map second) (map m) (map mid)))]
  (println "Part A:" (transduce (xf first identity) + xs))
  (println "Part B:" (transduce (xf (complement first)
                                    (partial sort (compare-fn ru)))
                                + xs)))
