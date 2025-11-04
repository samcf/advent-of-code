(def parse (map (partial re-seq #"\w+")))
(def valid (filter (partial apply distinct?)))

(defn anagram? [xs]
  (apply (complement distinct?) (map frequencies xs)))

(let [in (line-seq (java.io.BufferedReader. *in*))]
  (println "Part A:" (count (sequence (comp parse valid) in)))
  (println "Part B:" (count (sequence (comp parse valid (remove anagram?)) in))))
