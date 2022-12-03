(require '[clojure.set :refer [intersection]])

(def priority
  (->> (range 1 53)
       (interleave "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ")
       (apply hash-map)))

(def score-xf
  (comp (map (partial map set))
        (map (partial apply intersection))
        (map first)
        (map priority)))

(let [lns (line-seq (java.io.BufferedReader. *in*))]
  (println "Part A:" (transduce (comp (map #(split-at (/ (count %) 2) %)) score-xf) + lns))
  (println "Part B:" (transduce (comp (partition-all 3) score-xf) + lns)))
