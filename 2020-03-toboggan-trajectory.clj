(defn step-fn [x y w]
  (fn [n] (+ (- (mod (+ n x) w) (mod n w)) (* y w) n)))

(defn mark-trees [board]
  (comp (map (fn [indx] (get board indx)))
        (take-while some?)
        (map (fn [char] (if (= char \#) 1 0)))))

(defn solve [board width & slopes]
  (let [xf (mark-trees board)]
    (transduce (map (fn [indxs] (transduce xf + indxs))) *
               (map (fn [[x y]] (iterate (step-fn x y width) 0))
                    slopes))))

(let [ln (->> (java.io.BufferedReader. *in*) (line-seq) (apply str))]
  (println "Part A:" (solve ln 31 [3 1]))
  (println "Part B:" (solve ln 31 [1 1] [3 1] [5 1] [7 1] [1 2])))
