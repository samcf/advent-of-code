(ns advent
  (:require [clojure.string :refer [split]]))

(defn cost-a [a b] (Math/abs (- a b)))
(defn cost-b [a b] (let [n (Math/abs (- a b))] (* (/ n 2) (+ n 1))))

(defn solve [xs cost-fn]
  (->> (into #{} xs)
       (reduce (fn [c r] (update c r concat (map (partial cost-fn r) xs))) {})
       (into (sorted-set) (map (fn [[_ xs]] (reduce + 0 xs))))
       (first) (int)))

(let [line (first (line-seq (java.io.BufferedReader. *in*)))
      nums (into [] (map (fn [x] (Integer. x)) (split line #",")))]
  (println "Part A:" (solve nums cost-a))
  (println "Part B:" (solve nums cost-b)))
