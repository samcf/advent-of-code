(defn parse
  ([] {:ranges (sorted-set) :ids []})
  ([rs ln]
   (condp re-find ln
     #"(\d+)-(\d+)" :>> (fn [[_ a b]] (update rs :ranges conj [(parse-long a) (parse-long b)]))
     #"(\d+)"       :>> (fn [[_   a]] (update rs :ids    conj (parse-long a)))
     rs)))

(defn fresh [xs id]
  (some (fn [[a b]] (and (>= id a) (<= id b))) xs))

(defn disjoint [[[a b] :as xs] [c d :as x]]
  (if (<= (max a c) (min b d))
    (conj (rest xs) [(min a c) (max b d)])
    (conj xs x)))

(let [in (line-seq (java.io.BufferedReader. *in*))
      rs (reduce parse (parse) in)
      xs (reduce disjoint (list (first (:ranges rs))) (:ranges rs))]
  (println "Part A:" (transduce (keep (fn [id] (when (fresh xs id) 1))) + (:ids rs)))
  (println "Part B:" (transduce (map (fn [[a b]] (inc (- b a)))) + xs)))
