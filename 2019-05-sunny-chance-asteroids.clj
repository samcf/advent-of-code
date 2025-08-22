(defn fill [xs n]
  (concat (repeat (- n (count xs)) 0) xs))

(defn split [x]
  (loop [x x rs (list)]
    (if (> x 0) (recur (quot x 10) (conj rs (mod x 10)))
        rs)))

(defn run [xs id]
  (let [xs (transient xs)
        <- (fn [idx mode] (if (= mode 0) (xs (xs idx)) (xs idx)))]
    (loop [code 0 idx 0 xs xs]
      (let [[_ f e _ instr] (fill (split (xs idx)) 5)
            a (+ idx 1)
            b (+ idx 2)
            c (+ idx 3)
            d (+ idx 4)]
        (case instr
          1 (recur code d (assoc! xs (xs c) (+ (<- a e) (<- b f))))
          2 (recur code d (assoc! xs (xs c) (* (<- a e) (<- b f))))
          3 (recur code b (assoc! xs (xs a) id))
          4 (recur (<- a e) b xs)
          5 (recur code (if (not= (<- a e) 0) (<- b f) c) xs)
          6 (recur code (if (=    (<- a e) 0) (<- b f) c) xs)
          7 (recur code d (assoc! xs (xs c) (if (< (<- a e) (<- b f)) 1 0)))
          8 (recur code d (assoc! xs (xs c) (if (= (<- a e) (<- b f)) 1 0)))
          9 code)))))

(let [in (first (line-seq (java.io.BufferedReader. *in*)))
      xs (into [] (map parse-long) (re-seq #"-?\d+" in))]
  (println "Part A:" (run xs 1))
  (println "Part B:" (run xs 5)))
