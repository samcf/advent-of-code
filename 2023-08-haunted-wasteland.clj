(defn lcm [a b]
  (loop [x a]
    (if (> (mod x b) 0)
      (recur (+ x a)) x)))

(defn steps [graph ins pred start]
  (loop [ins (cycle ins) node start steps 0]
    (if (not (pred node))
      (recur (rest ins) (graph [node (first ins)]) (inc steps))
      steps)))

(defn branches [[x l r]]
  [[[x \L] l] [[x \R] r]])

(let [[xs _ & ys] (line-seq (java.io.BufferedReader. *in*))
      parse (comp (map (partial re-seq #"[A-Z0-9]+")) (mapcat branches))
      graph (into {} parse ys)
      steps (partial steps graph xs)
      solve (comp (map key) (map first)
                  (filter (comp #{\A} last))
                  (map (partial steps (comp #{\Z} last))))]
  (println "Part A:" (steps #{"ZZZ"} "AAA"))
  (println "Part B:" (transduce solve (completing lcm identity) 1 graph)))
