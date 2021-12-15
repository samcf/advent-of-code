(ns advent
  (:require [clojure.string :refer [split]]))

(defn inc- [x] (+ (or x 0) 1))

(defn fish [init days]
  (->> (range 7 (+ days 1))
       (reduce (fn [m d] (assoc m d (+ (get m (- d 7) 0) (get m (- d 9) 0)))) init)
       (transduce (map val) + 0)))

(let [line (first (line-seq (java.io.BufferedReader. *in*)))
      nums (into [] (map (fn [x] (Integer. x))) (split line #","))
      init (reduce (fn [m x] (update m (inc x) inc-)) {} nums)]
  (println "Part A:" (+ (fish init 80) (count nums)))
  (println "Part B:" (+ (fish init 256) (count nums))))
