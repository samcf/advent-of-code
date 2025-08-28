(ns permutations)

(defn- without [xs n]
  (keep-indexed (fn [i x] (when (not= i n) x)) xs))

(defn ^:export permutations [xs]
  (let [xs (seq xs)]
    (if (not xs)
      (list (list))
      (if (= (count xs) 1)
        (list xs)
        (into
         (list)
         (comp
          (map-indexed vector)
          (mapcat
           (fn [[i x]]
             (map
              (fn [xs] (conj xs x))
              (permutations (without xs i)))))) xs)))))
