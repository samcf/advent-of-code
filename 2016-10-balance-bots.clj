(defn parse [rs ln]
  (condp re-find ln
    #"value (\d+) .*? bot (\d+)"
    :>> (fn [[_   a b]] (update-in rs [:cur b] (fnil conj []) (parse-long a)))
    #"bot (\d+) .*? bot (\d+) .*? bot (\d+)"
    :>> (fn [[_ a b c]] (update rs :ins assoc a [b c]))
    #"bot (\d+) .*? bot (\d+) .*? output (\d+)"
    :>> (fn [[_ a b c]] (update rs :ins assoc a [b (str \+ c)]))
    #"bot (\d+) .*? output (\d+) .*? bot (\d+)"
    :>> (fn [[_ a b c]] (update rs :ins assoc a [(str \+ b) c]))
    #"bot (\d+) .*? output (\d+) .*? output (\d+)"
    :>> (fn [[_ a b c]] (update rs :ins assoc a [(str \+ b) (str \+ c)]))))

(defn solve [cur ins halt?]
  (if-let [[id [b c]] (first (filter (comp #{2} count val) cur))]
    (let [x (min b c)
          y (max b c)]
      (if (and halt? (= x 17) (= y 61)) id
          (let [[d e] (ins id)]
            (recur (dissoc (merge-with into cur {d [x] e [y]}) id) ins halt?))))
    (->> (select-keys cur ["+0" "+1" "+2"]) (vals) (map first) (reduce *))))

(let [in (line-seq (java.io.BufferedReader. *in*))
      rs (reduce parse {} in)]
  (println "Part A:" (solve (:cur rs) (:ins rs) true))
  (println "Part B:" (solve (:cur rs) (:ins rs) false)))
