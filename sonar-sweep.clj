(ns advent)

(def depths
  (into [] (map #(Integer. %)) (line-seq (java.io.BufferedReader. *in*))))

(defn a []
  (->> (partition 2 1 depths)
       (map (partial apply <))
       (filter identity)
       (count)))

(defn b []
  (->> (partition 3 1 depths)
       (map (partial apply +))
       (partition 2 1)
       (map (partial apply <))
       (filter identity)
       (count)))

(println (a))
(println (b))
