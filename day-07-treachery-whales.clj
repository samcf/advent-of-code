(ns advent
  (:require [clojure.string :refer [split]]))

(defn cost-a [a b] (Math/abs (- a b)))
(defn solve-a [xs]
  (let [xs (into [] (sort xs)) m (xs (/ (count xs) 2))]
    (transduce (map (fn [x] (cost-a x m))) + 0 xs)))

(defn cost-b [a b] (let [n (Math/abs (- a b))] (int (* (/ n 2) (+ n 1)))))
(defn solve-b [xs]
  (let [m (int (/ (reduce + xs) (count xs)))]
    (transduce (map (fn [x] (cost-b x m))) + 0 xs)))

(let [line (first (line-seq (java.io.BufferedReader. *in*)))
      nums (into [] (map (fn [x] (Integer. x)) (split line #",")))]
  (println "Part A:" (solve-a nums))
  (println "Part B:" (solve-b nums)))
