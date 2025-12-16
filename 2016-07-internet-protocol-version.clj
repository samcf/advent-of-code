(defn tls? [xs]
  (loop [idx 0 ext false hyp false a nil b nil c nil]
    (if (< idx (count xs))
      (let [d (xs idx) match (and (= a d) (= b c) (not= a b))]
        (cond
          (= d \[) (recur (inc idx) ext true nil nil nil)
          (= d \]) (recur (inc idx) ext false nil nil nil)
          (and match hyp) false
          match (recur (inc idx) true hyp b c d)
          :else (recur (inc idx) ext hyp b c d)))
      ext)))

(defn ssl? [xs]
  (loop [idx 0 ext (list) int (list) hyp false a nil b nil]
    (if (< idx (count xs))
      (let [c (xs idx) match (and (= a c) (not= a b))]
        (cond
          (= c \[) (recur (inc idx) ext int true nil nil)
          (= c \]) (recur (inc idx) ext int false nil nil)
          (and match hyp) (recur (inc idx) ext (conj int [a b c]) hyp b c)
          match (recur (inc idx) (conj ext [a b c]) int hyp b c)
          :else (recur (inc idx) ext int hyp b c)))
      (some
       (fn [[a b c]]
         (some
          (fn [[d e f]]
            (and (= a e c) (= b d f))) ext)) int))))

(defn counting [pred]
  (keep (fn [x] (when (pred x) 1))))

(let [in (line-seq (java.io.BufferedReader. *in*))]
  (println "Part A:" (transduce (comp (map vec) (counting tls?)) + in))
  (println "Part B:" (transduce (comp (map vec) (counting ssl?)) + in)))
