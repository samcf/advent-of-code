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
      solve (fn [n] (reduce (fn [[vis crd] [ox oy]]
                              (let [[ax ay] (first crd)
                                    [dx dy] [(+ ax ox) (+ ay oy)]]
                                [(conj vis (last crd))
                                 (reduce (fn [crd [idx vct]] (assoc crd (inc idx) (follow vct (crd idx))))
                                         (assoc crd 0 [dx dy])
                                         (map-indexed vector (rest crd)))]))
                            [#{[0 0]} (vec (repeat n [0 0]))]
                            moves))]
  (println "Part A:" (count (first (solve 2))))
  (println "Part B:" (count (first (solve 10)))))
