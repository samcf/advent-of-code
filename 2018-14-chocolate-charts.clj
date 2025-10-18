(defn split [x]
  (if (zero? x) (list 0)
      (loop [x x rs (list)]
        (if (> x 0) (recur (quot x 10) (conj rs (mod x 10)))
            rs))))

(defn move [xs idx]
  (mod (+ idx (xs idx) 1)
       (count xs)))

(defn scores [xs f]
  (loop [xs xs a 0 b 1]
    (if-let [rs (f xs)]
      rs
      (let [xs (into xs (split (+ (xs a) (xs b))))]
        (recur xs (move xs a) (move xs b))))))

(defn after [t n]
  (fn [xs]
    (when (> (count xs) (+ t n))
      (apply str (subvec xs t (+ t n))))))

(defn before [ps]
  (fn [xs]
    (let [s (- (count xs) (count ps))]
      (condp = ps
        (subvec xs (max s 0)) s
        (subvec xs (max (dec s) 0) (dec (count xs))) (dec s)
        nil))))

(let [in (first (line-seq (java.io.BufferedReader. *in*)))
      xs (into [] (comp (map str) (map parse-long)) in)]
  (println "Part A:" (scores [3 7] (after (parse-long in) 10)))
  (println "Part B:" (scores [3 7] (before xs))))
