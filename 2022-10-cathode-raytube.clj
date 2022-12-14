(require '[clojure.string :refer [join]])

(def render
  (comp (map-indexed vector)
        (map (fn [[idx val]]
               (if (<= val idx (+ val 2))
                 \█ \░)))))

(defn solve [xs ln]
  (let [prev (peek xs)]
    (condp re-find ln
      #"noop"              (conj xs prev)
      #"addx (-?\d+)" :>> #(conj xs prev (+ prev (Integer. (second %)))))))

(let [in (line-seq (java.io.BufferedReader. *in*))
      xs (reduce solve [1 1] in)]
  (println "Part A:" (transduce (map (fn [x] (* (xs x) x))) + [20 60 100 140 180 220]))
  (println "Part B:")
  (println (->> (partition 40 xs)
                (map #(join "" (sequence render %)))
                (join "\n"))))
