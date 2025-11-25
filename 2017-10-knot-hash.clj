(require '[knot :as knot])

(let [in (first (line-seq (java.io.BufferedReader. *in*)))
      xs (re-seq #"\d+" in)]
  (println "Part A:" (reduce * (take 2 (transduce (map parse-long) knot/rf xs))))
  (println "Part B:" (transduce knot/encode str (knot/create in))))
