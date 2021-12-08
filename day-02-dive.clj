(ns advent)

(defn a [coll]
  (->> (reduce
        (fn [[h d] [c n]]
          (case c
            "forward" [(+ h n) d]
            "down"    [h (+ d n)]
            "up"      [h (- d n)])) [0 0] coll)
       (apply *)))

(defn b [coll]
  (->> (reduce
        (fn [[h d a] [c n]]
          (case c
            "forward" [(+ h n) (+ d (* a n)) a]
            "down"    [h d (+ a n)]
            "up"      [h d (- a n)])) [0 0 0] coll)
       (take 2)
       (apply *)))

(def commands
  (into []
        (map #(let [[c n] (clojure.string/split % #" ")] [c (Integer. n)]))
        (line-seq (java.io.BufferedReader. *in*))))

(println "Part A:" (a commands))
(println "Part B:" (b commands))
