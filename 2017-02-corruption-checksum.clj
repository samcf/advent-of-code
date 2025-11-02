(def parse
  (comp
   (map (fn [ln] (re-seq #"\d+" ln)))
   (map (fn [xs] (map parse-long xs)))))

(def diff
  (comp
   (map sort)
   (map (juxt last first))
   (map (partial reduce -))))

(defn divide [xs]
  (first
   (for [a xs b xs :when (and (not= a b) (zero? (rem a b)))]
     (/ a b))))

(let [in (line-seq (java.io.BufferedReader. *in*))]
  (println "Part A:" (transduce (comp parse diff) + in))
  (println "Part B:" (transduce (comp parse (map divide)) + in)))
