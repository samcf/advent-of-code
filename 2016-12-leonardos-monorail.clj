(defn parse [ln]
  (condp re-find ln
    #"cpy (\d+) (\w)"    :>> (fn [[_ a b]] ['cpy  (parse-long a) (first b)])
    #"cpy (\w) (\w)"     :>> (fn [[_ a b]] ['cpy' (first a) (first b)])
    #"inc (\w)"          :>> (fn [[_   a]] ['inc  (first a)])
    #"dec (\w)"          :>> (fn [[_   a]] ['dec  (first a)])
    #"jnz (\d+) (-?\d+)" :>> (fn [[_ a b]] ['jnz  (parse-long a) (parse-long b)])
    #"jnz (\w) (-?\d+)"  :>> (fn [[_ a b]] ['jnz' (first a) (parse-long b)])))

(defn evaluate [init xs]
  (loop [reg init idx 0]
    (if-let [[ins a b] (get xs idx)]
      (case ins
        cpy  (recur (assoc reg b a) (inc idx))
        cpy' (recur (assoc reg b (get reg a 0)) (inc idx))
        inc  (recur (update reg a inc) (inc idx))
        dec  (recur (update reg a dec) (inc idx))
        jnz  (if (not (zero? a)) (recur reg (+ idx b)) (recur reg (inc idx)))
        jnz' (if (not (zero? (get reg a 0))) (recur reg (+ idx b)) (recur reg (inc idx))))
      reg)))

(let [in (line-seq (java.io.BufferedReader. *in*))
      xs (into [] (map parse) in)]
  (println "Part A:" (get (evaluate {}     xs) \a))
  (println "Part B:" (get (evaluate {\c 1} xs) \a)))
