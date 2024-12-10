(def expand
  (comp (map str)
        (map parse-long)
        (partition-all 2)
        (map-indexed vector)
        (mapcat
         (fn [[i [u f]]]
           (into (repeat f nil) (repeat u i))))))

(defn checksum-a [xs]
  (let [xr (reverse (filter identity xs))
        xc (count xr)]
    (loop [[x & xs] xs ;; blocks
           rev xr      ;; blocks (reverse)
           sum 0       ;; checksum
           idx 0       ;; disk index
           rep 0       ;; blocks replaced
           num 0       ;; blocks counted
           ]
      (if (< rep (- xc num))
        (if (nil? x)
          (recur xs (rest rev) (+ sum (* idx (first rev))) (inc idx) (inc rep) num)
          (recur xs rev (+ sum (* idx x)) (inc idx) rep (inc num)))
        sum))))

(defn checksum-b [_]
  1)

(let [in (str (first (line-seq (java.io.BufferedReader. *in*))) 0)
      xs (into [] expand in)]
  (println "Part A:" (checksum-a xs))
  (println "Part B:" (checksum-b xs)))
