(ns advent
  (:require [clojure.string :refer [split triml]]
            [clojure.set :refer [intersection]]))

(def masks [0x1F00000 0xF8000 0x7C00 0x3E0 0x1F 0x1084210 0x842108 0x421084 0x210842 0x108421])

(defn scores
  "Returns a sorted map whose keys are scores and whose values are the sum of
   unmarked numbers multiplied by the number that was called when the board
   was completed."
  [boards xs]
  (reduce
   (fn [totals board]
     (apply assoc totals
            (reduce
             (fn [[prev xs] x]
               (if-let [index (board x)]
                 (let [next (bit-or prev (bit-shift-left 1 (- 24 index)))]
                   (if (some #(= (bit-and next %) %) masks)
                     (reduced
                      [(count xs)
                       (->> (intersection (set (rest xs)) (set (keys board))) (reduce +) (* x))])
                     [next (rest xs)]))
                 [prev (rest xs)]))
             [0 xs] xs)))
   (sorted-map) boards))

(def parse-xf
  (comp (filter seq)
        (map triml)
        (map (fn [line] (split line #"\s+")))
        (partition-all 5)
        (map flatten)
        (map (fn [xs] (map #(Integer. %) xs)))
        (map (fn [xs] (map-indexed (comp vec rseq vector) xs)))
        (map (partial into {}))))

(def input
  (let [[xs & bs] (into [] (line-seq (java.io.BufferedReader. *in*)))]
    [(into [] parse-xf bs) (map #(Integer. %) (split xs #","))]))

(let [scored (apply scores input)]
  (println "Part A:" (val (last scored)))
  (println "Part B:" (val (first scored))))
