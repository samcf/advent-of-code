(require '[clojure.string :refer [split]])

(defn dive-a [[h d] [c n]]
  (case c
    "forward" [(+ h n) d]
    "down"    [h (+ d n)]
    "up"      [h (- d n)]))

(defn dive-b [[h d a] [c n]]
  (case c
    "forward" [(+ h n) (+ d (* a n)) a]
    "down"    [h d (+ a n)]
    "up"      [h d (- a n)]))

(let [in (line-seq (java.io.BufferedReader. *in*))
      cf (fn [[h d]] (* h d))
      xf (comp (map (fn [s]     (split s #" ")))
               (map (fn [[c v]] [c (Integer. v)])))]
  (println "Part A:" (transduce xf (completing dive-a cf) [0 0]   in))
  (println "Part B:" (transduce xf (completing dive-b cf) [0 0 0] in)))
