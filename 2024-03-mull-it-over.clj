(def re-mul #"mul\((\d+)\,(\d+)\)")
(def re-all #"(don't)|(do)|mul\((\d+)\,(\d+)\)")

(let [in (line-seq (java.io.BufferedReader. *in*))
      xs (sequence (mapcat (fn [ln] (re-seq re-all ln))) in)
      xf (comp (mapcat (fn [ln] (re-seq re-mul ln)))
               (map rest)
               (map (fn [xs] (map parse-long xs)))
               (map (fn [xs] (reduce * xs))))]
  (println "Part A:" (transduce xf + in))
  (println "Part B:"
           (loop [xs xs sum 0 flag true]
             (if (seq xs)
               (let [[[_ a b x y] & xs] xs]
                 (cond a (recur xs sum false)
                       b (recur xs sum true)
                       x (if flag
                           (recur xs (+ sum (* (parse-long x) (parse-long y))) flag)
                           (recur xs sum flag)))) sum))))
