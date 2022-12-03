(ns advent
  (:require [clojure.set :refer [intersection]]))

(def priority
  (into {} (comp (partition-all 2) (map (fn [[c i]] [(char c) i])))
        (interleave (concat (range 65 91) (range 97 124)) (range 1 53))))

(def sum-xf
  (comp (map (partial map set))
        (map (partial apply intersection))
        (map first)
        (map priority)))

(let [lns (line-seq (java.io.BufferedReader. *in*))]
  (println "Part A:" (transduce (comp (map #(partition (/ (count %) 2) %)) sum-xf) + lns))
  (println "Part B:" (transduce (comp (partition-all 3) sum-xf) + lns)))
