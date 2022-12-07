(defn walk-and-sum [[tree path] ln]
  (condp re-find ln
    #"\$ cd /"     [tree [["/"]]]
    #"\$ cd \.\."  [tree (pop path)]
    #"\$ cd (\S+)" :>> (fn [[_ dir]] [tree (conj path (conj (peek path) dir))])
    #"(\d+)"       :>> (fn [[_ siz]] [(reduce (fn [tree dir] (update tree dir #(+ (or %1 0) %2) (Integer. siz))) tree path) path])
    [tree path]))

(let [lns      (line-seq (java.io.BufferedReader. *in*))
      [tree _] (reduce walk-and-sum [{["/"] 0} []] lns)]
  (println "Part A:" (transduce (comp (filter #(<= (second %) 100000)) (map val)) + tree))
  (println "Part B:" (->> (filter (fn [[_ size]] (->> (tree ["/"]) (- 70000000) (- 30000000) (>= size))) tree)
                          (apply min-key second)
                          (val))))
