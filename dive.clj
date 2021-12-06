(ns advent)

(def commands
  (into []
        (map (let [[c n] (clojure.string/split % #" ")] [c (Integer. n)]))
        (line-seq (java.io.BufferedReader. *in*))))

(defn a []
  (->> (reduce
        (fn [[h d] [c n]]
          (case c
            "forward" [(+ h n) d]
            "down"    [h (+ d n)]
            "up"      [h (- d n)])) [0 0] commands)
       (apply *)))

(defn b []
  (->> (reduce
        (fn [[h d a] [c n]]
          (case c
            "forward" [(+ h n) (+ d (* a n)) a]
            "down"    [h d (+ a n)]
            "up"      [h d (- a n)])) [0 0 0] commands)
       (take 2)
       (apply *)))

(a)
(b)
