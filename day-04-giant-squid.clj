(ns advent
  (:require [clojure.string :refer [split triml]]
            [clojure.set :refer [intersection]]))

(def masks [0x1F00000 0xF8000 0x7C00 0x3E0 0x1F 0x1084210 0x842108 0x421084 0x210842 0x108421])

(defn scores
  "Returns a sequence of bingo board scores ordered from last to
   complete to first."
  [boards xs]
  (->> (map
        (fn [board]
          (reduce
           (fn [[prev xs] x]
             (if-let [index (get board x)]
               (let [mask (bit-shift-left 1 (- 24 index))
                     next (bit-or prev mask)]
                 (if (some #(= (bit-and next %) %) masks)
                   (let [unmarked (intersection (set (keys board)) (set (rest xs)))]
                     (reduced [(count xs) (reduce + unmarked) x]))
                   [next (rest xs)]))
               [prev (rest xs)]))
           [0 xs] xs)) boards)
       (sort-by first)
       (map (juxt second last))
       (map (partial reduce *))))

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
  (println "Part A:" (last scored))
  (println "Part B:" (first scored)))
