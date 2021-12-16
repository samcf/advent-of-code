(ns advent
  (:require [clojure.string :refer [split]]))

(defn solve [xs days]
  (->> (range 7 (inc days))
       (reduce (fn [m x] (assoc m x (+ (get m (- x 7) 0) (get m (- x 9) 0))))
               (frequencies xs))
       (transduce (map val) + 0)
       (+ (count xs))))

(let [ln (first (line-seq (java.io.BufferedReader. *in*)))
      xs (into [] (comp (map #(Integer. %)) (map inc)) (split ln #","))]
  (println "Part A:" (solve xs 80))
  (println "Part B:" (solve xs 256)))
