(defn converges [a b]
  (loop [x a]
    (if (> (mod x b) 0)
      (recur (+ x a)) x)))

(defn steps [graph instr pred start]
  (loop [xs (seq instr) node start steps 0]
    (if (not (pred node))
      (if (seq xs)
        (let [[dir & xs] xs]
          (recur xs (get graph [node dir]) (inc steps)))
        (recur (seq instr) node steps)) steps)))

(defn create-branch [[x l r]]
  [[[x \L] l] [[x \R] r]])

(let [[xs _ & ys] (line-seq (java.io.BufferedReader. *in*))
      parse (comp (map (partial re-seq #"[A-Z0-9]+"))
                  (mapcat create-branch))
      graph (into {} parse ys)
      steps (partial steps graph xs)]
  (println "Part A:" (steps #{"ZZZ"} "AAA"))
  (println "Part B:" (transduce (map (partial steps (comp #{\Z} last)))
                                (completing converges identity) 1
                                (sequence (comp (map (comp first key))
                                                (filter (comp #{\A} last)))
                                          graph))))
