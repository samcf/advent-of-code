(let [lns      (line-seq (java.io.BufferedReader. *in*))
      [tree _] (reduce (fn [[tree path] ln]
                         (condp re-find ln
                           #"\$ cd /" :>>     (fn [_] [tree ["/"]])
                           #"\$ cd \.\." :>>  (fn [_] [tree (pop path)])
                           #"\$ cd (\S+)" :>> (fn [[_ dir]] [tree (conj path (conj path dir))])
                           #"(\d+) (\S+)" :>> (fn [[_ size _]] [(reduce (fn [tree dir] (update tree dir #(+ (or %1 0) %2) (Integer. size))) tree path) path])
                           [tree path]))
                       [{"/" 0} []] lns)]
  (println "Part A:" (transduce (comp (filter #(<= (second %) 100000)) (map val)) + tree))
  (println "Part B:" (->> tree
                          (filter (fn [[_ size]] (->> (tree "/") (- 70000000) (- 30000000) (>= size))))
                          (apply min-key second)
                          val)))
