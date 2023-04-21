(require '[clojure.set :refer [union intersection]])

(let [in (line-seq (java.io.BufferedReader. *in*))
      xf (fn [f]
           (comp (partition-by (partial = ""))
                 (filter       (partial not= [""]))
                 (map          (partial map (partial into (hash-set))))
                 (map          (partial apply f))
                 (map          count)))]
  (println "Part A:" (transduce (xf union)        + in))
  (println "Part B:" (transduce (xf intersection) + in)))
