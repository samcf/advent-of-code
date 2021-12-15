(ns advent
  (:require [clojure.string :refer [split]]))

(defn straight? [[ax ay bx by]]
  (or (= ax bx) (= ay by)))

(defn spread [src dst]
  (if (> src dst) (range src (- dst 1) -1) (range src (+ dst 1))))

(defn points [[ax ay bx by]]
  (cond (= ax bx) (map (fn [v] [ax v]) (spread ay by))
        (= ay by) (map (fn [v] [v ay]) (spread ax bx))
        :else (mapv vector (spread ax bx) (spread ay by))))

(def dupes-xf (comp (map val) (filter (fn [x] (> x 1))) (map (constantly 1))))

(defn solve [xs]
  (->> (sequence (comp (map points) cat) xs)
       (frequencies)
       (transduce dupes-xf + 0)))

(def vecs-xf (comp (map (fn [xs] (split xs #","))) cat (map (fn [x] (Integer. x)))))
(def line-xf (comp (map (fn [ls] (split ls #" -> "))) (map (fn [xs] (into [] vecs-xf xs)))))

(let [lines (line-seq (java.io.BufferedReader. *in*))
      parse (into [] line-xf lines)]
  (println "Part A:" (solve (filter straight? parse)))
  (println "Part B:" (solve parse)))
