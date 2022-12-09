(defn parse-ln [ln]
  (condp re-find ln
    #"U (\d+)" :>> (fn [[_ n]] (repeat (Integer. n) [0 -1]))
    #"R (\d+)" :>> (fn [[_ n]] (repeat (Integer. n) [1  0]))
    #"D (\d+)" :>> (fn [[_ n]] (repeat (Integer. n) [0  1]))
    #"L (\d+)" :>> (fn [[_ n]] (repeat (Integer. n) [-1 0]))))

(defn follow [[ax ay] [bx by]]
  (if (> (max (Math/abs (- ax bx)) (Math/abs (- ay by))) 1)
    [(+ (Integer/signum (- bx ax)) ax)
     (+ (Integer/signum (- by ay)) ay)]
    [ax ay]))

(let [lines (line-seq (java.io.BufferedReader. *in*))
      moves (sequence (comp (map parse-ln) cat) lines)
      solve #(loop [knots   (vec (repeat % [0 0]))
                    offsets moves
                    visited #{}]
               (if (seq offsets)
                 (let [[ax ay] (first knots)
                       [ox oy] (first offsets)]
                   (recur (loop [knots (assoc knots 0 [(+ ax ox) (+ ay oy)])
                                 idx   1]
                            (if (< idx (count knots))
                              (recur (update knots idx follow (knots (dec idx)))
                                     (inc idx))
                              knots))
                          (rest offsets)
                          (conj visited (last knots))))
                 (count visited)))]
  (println "Part A:" (solve 2))
  (println "Part B:" (solve 10)))
