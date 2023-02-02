(defn parse [s]
  (let [[_ a b c s] (re-find #"(\d+)-(\d+) ([a-z]): ([a-z]+)" s)]
    [(Integer. a) (Integer. b) (first c) s]))

(defn policy-a [[a b c s]]
  (let [n (or ((frequencies s) c) 0)]
    (<= a n b)))

(defn policy-b [[a b c s]]
  (not= (= (get s (- a 1)) c)
        (= (get s (- b 1)) c)))

(let [lns (map parse (line-seq (java.io.BufferedReader. *in*)))
      cmp (map (fn [x] (if x 1 0)))]
  (println "Part A:" (transduce (comp (map policy-a) cmp) + lns))
  (println "Part B:" (transduce (comp (map policy-b) cmp) + lns)))
