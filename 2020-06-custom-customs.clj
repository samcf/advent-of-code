(require '[clojure.set :refer [union intersection]])

(let [in (line-seq (java.io.BufferedReader. *in*))
      xf (comp (partition-by (partial = ""))
               (filter       (partial not= [""]))
               (map          (partial map (partial into (hash-set)))))
      xa (comp xf (map (partial apply union)) (map count))
      xb (comp xf (map (partial apply intersection)) (map count))]
  (println "Part A:" (transduce xa + in))
  (println "Part B:" (transduce xb + in)))
