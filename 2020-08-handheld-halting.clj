(defn parse [ln]
  (let [[_ ins val] (re-find #"(\w+) ((?:\+|-)\d+)" ln)]
    [ins (Integer. val)]))

(defn solve-a [xs]
  (loop [acc 0 idx 0 vst #{}]
    (if (not (vst idx))
      (let [[ins val] (xs idx) vst (conj vst idx)]
        (case ins
          "nop" (recur acc         (inc idx)   vst)
          "acc" (recur (+ acc val) (inc idx)   vst)
          "jmp" (recur acc         (+ idx val) vst)))
      acc)))

(defn solve-b [xs]
  (loop [acc 0    ;; value of the accumulator
         idx 0    ;; current instruction index
         vst #{}  ;; set of all indexes visited per test
         bad #{}  ;; set of all indexes which failed its test
         tst nil  ;; the index that is currently being tested
         ]
    (if (<= (inc idx) (count xs))
      (if (not (vst idx))
        (let [[ins val] (xs idx)
              vst       (conj vst idx)
              run       (or (number? tst) (contains? bad idx))]
          (case [ins run]
            ["acc" true]  (recur (+ acc val) (inc idx)   vst bad tst)
            ["nop" true]  (recur acc         (inc idx)   vst bad tst)
            ["jmp" true]  (recur acc         (+ idx val) vst bad tst)
            ["acc" false] (recur (+ acc val) (inc idx)   vst bad tst)
            ["nop" false] (recur acc         (+ idx val) vst bad idx)
            ["jmp" false] (recur acc         (inc idx)   vst bad idx)))
        (recur 0 0 #{} (conj bad tst) nil))
      acc)))

(let [in (line-seq (java.io.BufferedReader. *in*))
      xs (into [] (map parse) in)]
  (println "Part A:" (solve-a xs))
  (println "Part B:" (solve-b xs)))
