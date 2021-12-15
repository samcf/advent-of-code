(ns advent
  (:require [clojure.string :refer [split]]))

(defn cost-a [a b] (Math/abs (- a b)))
(defn solve-a [xs]
  (let [xs (into [] (sort xs)) m (xs (/ (count xs) 2))]
    (transduce (map (partial cost-a m)) + 0 xs)))

(defn cost-b [a b] (let [n (Math/abs (- a b))] (int (* (/ n 2) (+ n 1)))))
(defn solve-b [xs]
  (let [m (int (/ (reduce + xs) (count xs)))]
    (transduce (map (partial cost-b m)) + 0 xs)))

(let [ln (first (line-seq (java.io.BufferedReader. *in*)))
      xs (into [] (map (fn [x] (Integer. x))) (split ln #","))]
  (println "Part A:" (solve-a xs))
  (println "Part B:" (solve-b xs)))
