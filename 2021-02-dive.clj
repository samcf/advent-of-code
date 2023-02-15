(require '[clojure.string :refer [split]])

(defn solve-a [[[c v] & t] h d]
  (if (seq t)
    (case c
      "forward" (recur t (+ h v) d)
      "down"    (recur t h (+ d v))
      "up"      (recur t h (- d v)))
    (* h d)))

(defn solve-b [[[c v] & t] h d a]
  (if (seq t)
    (case c
      "forward" (recur t (+ h v) (+ d (* a v)) a)
      "down"    (recur t h d (+ a v))
      "up"      (recur t h d (- a v)))
    (* h d)))

(let [in (line-seq (java.io.BufferedReader. *in*))
      xf (comp (map (fn [s]     (split s #" ")))
               (map (fn [[c v]] [c (Integer. v)])))
      xs (sequence xf in)]
  (println "Part A:" (solve-a xs 0 0))
  (println "Part B:" (solve-b xs 0 0 0)))
