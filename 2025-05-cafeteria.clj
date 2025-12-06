(defn parse
  ([] {:ranges (hash-set) :ids []})
  ([m ln]
   (condp re-find ln
     #"(\d+)-(\d+)" :>> (fn [[_ a b]] (update m :ranges conj [(parse-long a) (parse-long b)]))
     #"(\d+)"       :>> (fn [[_   a]] (update m :ids    conj (parse-long a)))
     m)))

(defn fresh? [rs id]
  (some (fn [[a b]] (and (>= id a) (<= id b))) rs))

(defn overlap? [[a b]]
  (fn [[c d]]
    (<= (max a c) (min b d))))

(defn subset? [[a b]]
  (fn [[c d]]
    (and (>= c a) (<= d b))))

(defn split [[a b] [c d]]
  (let [mid (max a c)]
    [[(min a c) mid]
     [(inc mid) (max b d)]]))

(defn discrete [rs]
  (loop [rs rs sum 0]
    (if-let [a (first rs)]
      (let [rest (disj rs a)]
        (if-let [b (first (filter (subset? a) rest))]
          (recur (disj rs b) sum)
          (if-let [b (first (filter (overlap? a) rest))]
            (recur (into (disj rs a b) (split a b)) sum)
            (recur rest (+ sum (inc (- (a 1) (a 0))))))))
      sum)))

(let [in (line-seq (java.io.BufferedReader. *in*))
      rs (reduce parse (parse) in)
      xf (keep (fn [id] (when (fresh? (:ranges rs) id) 1)))]
  (println "Part A:" (transduce xf + (:ids rs)))
  (println "Part B:" (discrete (:ranges rs))))
