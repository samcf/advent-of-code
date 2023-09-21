(require '[clojure.set :refer [difference]])

(def re-rule #"([\w\s]+): (\d+)-(\d+) or (\d+)-(\d+)")
(def re-tick #"(\d+)")

(defn parse [s]
  (condp re-seq s
    re-rule :>> (fn [[[_ s & xs]]] (into [s] (map #(Integer. %)) xs))
    re-tick :>> (fn [xs] (sequence (map (fn [[_ x]] (Integer. x))) xs))
    nil))

(defn within? [[_ a b c d] x]
  (or (<= a x b) (<= c x d)))

(defn valid? [rules]
  (fn [x]
    (some (fn [rule] (within? rule x)) rules)))

(defn candidate? [xs]
  (fn [rule]
    (every? (fn [x] (within? rule x)) xs)))

(defn fields [columns rules]
  (let [xf (fn [xs] (comp (filter (candidate? xs)) (map first)))
        xs (sequence (comp (map (fn [xs] (into #{} (xf xs) rules))) (map-indexed vector)) columns)]
    (loop [poss (sort-by (comp count second) xs)
           path (into [] (repeat 20 0))
           seen #{}]
      (if (seq poss)
        (let [[indx rules] (first poss)
              rule (first (difference rules seen))]
          (recur (rest poss) (assoc path indx rule) (conj seen rule)))
        path))))

(let [in (line-seq (java.io.BufferedReader. *in*))
      xf (comp (map parse)
               (filter (complement nil?))
               (partition-by (comp string? first)))
      [rules [xs & ys]] (sequence xf in)
      vs (filter (partial every? (valid? rules)) ys)]
  (println "Part A:" (transduce (comp cat (filter (complement (valid? rules)))) + ys))
  (time (println "Part B:" (transduce (comp (filter (comp (partial re-find #"departure") key)) (map val)) *
                                      (-> (apply mapv vector vs)
                                          (fields rules)
                                          (zipmap xs))))))
