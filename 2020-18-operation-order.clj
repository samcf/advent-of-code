(def token {\( \( \) \) \+ \+ \* \* \1 1 \2 2 \3 3 \4 4 \5 5 \6 6 \7 7 \8 8 \9 9 \0 0})

(defn evaluate [xs]
  (loop [xs xs rs 0 op +]
    (if (seq xs)
      (let [[x & xs] xs]
        (case x
          \( (let [[rx xs] (evaluate xs)] (recur xs (op rs rx) op))
          \) [rs xs]
          \+ (recur xs rs +)
          \* (recur xs rs *)
          (recur xs (op rs x) op))) rs)))

(defn rewrite
  ([xs] (rewrite xs []))
  ([xs rs]
   (if (seq xs)
     (let [[x & xs] xs]
       (case x
         \( (let [[xs ys] (rewrite xs)] (recur xs (conj rs (evaluate ys))))
         \) [xs rs]
         \+ (if (= (first xs) \()
              (let [[xs ys] (rewrite (rest xs))]
                (recur xs (conj (pop rs) (+ (peek rs) (evaluate ys)))))
              (recur (rest xs) (conj (pop rs) (+ (peek rs) (first xs)))))
         (recur xs (conj rs x)))) rs)))

(let [in (line-seq (java.io.BufferedReader. *in*))
      pf (comp (filter (complement #{\space})) (map token))
      xf (fn [f] (comp (map (fn [xs] (sequence pf xs))) (map f) (map evaluate)))]
  (println "Part A:" (transduce (xf identity) + in))
  (println "Part B:" (transduce (xf rewrite) + in)))
