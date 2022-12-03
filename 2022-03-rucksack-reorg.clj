(ns advent
  (:require [clojure.set :refer [intersection]]))

(defn priority [c]
  (if (Character/isUpperCase c)
    (+ (- (int c) (int \A)) 27)
    (+ (- (int c) (int \a)) 1)))

(def score-xf
  (comp (map (partial map set))
        (map (partial apply intersection))
        (map first)
        (map priority)))

(let [lns (line-seq (java.io.BufferedReader. *in*))]
  (println "Part A:" (transduce (comp (map #(partition (/ (count %) 2) %)) score-xf) + lns))
  (println "Part B:" (transduce (comp (partition-all 3) score-xf) + lns)))
