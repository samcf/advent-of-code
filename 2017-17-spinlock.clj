(require '[clojure.core.rrb-vector :as rrb])

(defn insert [xs k v]
  (rrb/catvec
   (subvec xs 0 k) [v]
   (subvec xs k)))

(defn spin [step stop]
  (loop [xs [0] pos 0 run 1]
    (if (> run stop)
      (xs (inc pos))
      (let [pos (inc (mod (+ pos step) run))]
        (recur (insert xs pos run) pos (inc run))))))

(defn spin' [step stop]
  (loop [val nil pos 0 run 1]
    (if (> run stop) val
        (let [pos (inc (mod (+ pos step) run))]
          (if (= pos 1)
            (recur run pos (inc run))
            (recur val pos (inc run)))))))

(let [in (line-seq (java.io.BufferedReader. *in*))
      sx (parse-long (first in))]
  (println "Part A:" (spin  sx 2017))
  (println "Part B:" (spin' sx 5e7)))
