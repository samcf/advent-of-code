(require '[clojure.string :refer [join]])

(defn parse-ln [ln]
  (condp re-find ln
    #"noop"             [1 0]
    #"addx (-?\d+)" :>> (fn [[_ x]] [2 (Integer. x)])))

(def xf-render
  (comp (map-indexed vector)
        (map (fn [[idx val]]
               (if (<= val idx (+ val 2)) "#" ".")))))

(let [lns (line-seq (java.io.BufferedReader. *in*))
      ins (sequence (map parse-ln) lns)
      res (loop [res [1] reg 1 [[wait change] & ins] ins]
            (if (seq ins)
              (recur (apply conj res (repeat wait reg))
                     (+ reg change)
                     ins)
              res))]
  (println "Part A:"   (transduce (map #(* (res %) %)) + [20 60 100 140 180 220]))
  (println "Part B:\n" (->> (sequence (comp (partition-all 40)
                                            (map (partial transduce xf-render str)))
                                      res)
                            (join "\n"))))
