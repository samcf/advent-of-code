(require '[clojure.math :refer [ceil]])

(defn ways [[t r]]
  (loop [x (int (ceil (/ t 2))) y 0]
    (if (> (* x (- t x)) r)
      (recur (inc x) (inc y))
      (if (= (mod t 2) 0)
        (dec (* y 2))
        (* y 2)))))

(let [in (line-seq (java.io.BufferedReader. *in*))
      xf (comp (map (partial re-seq #"\d+")) (map (partial map parse-long)))
      yf (comp (map (partial re-seq #"\d+"))
               (map (partial apply str))
               (map parse-long))]
  (println "Part A:" (transduce (map ways) * (apply mapv vector (sequence xf in))))
  (println "Part B:" (ways (sequence yf in))))
