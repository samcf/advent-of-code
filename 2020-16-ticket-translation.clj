(def parse-rule #"([\w\s]+): (\d+)-(\d+) or (\d+)-(\d+)")
(def parse-tick #"(\d+)")

(defn parse [s]
  (condp re-seq s
    parse-rule :>> (fn [[[_ s & xs]]] (into [s] (map #(Integer. %)) xs))
    parse-tick :>> (fn [xs] (sequence (map (fn [[_ x]] (Integer. x))) xs)) ""))

(defn valid? [rules x]
  (if (seq rules)
    (let [[[_ a b c d] & rules] rules]
      (if (not (or (<= a x b) (<= c x d)))
        (recur rules x) true)) false))

(let [in (line-seq (java.io.BufferedReader. *in*))
      xf (comp (map parse)
               (filter (complement #{""}))
               (partition-by (comp string? first)))
      [rs [_ & nt]] (sequence xf in)
      xt (comp cat (filter (complement (partial valid? rs))))]
  (println "Part A:" (transduce xt + nt)))
