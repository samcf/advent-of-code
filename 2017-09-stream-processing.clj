(defn score [xs]
  (loop [xs xs dp 0 sk false rs 0]
    (if-let [[x & xs] (seq xs)]
      (cond (= x \!) (recur (rest xs) dp sk rs)
            (= x \>) (recur xs dp false rs)
            sk       (recur xs dp sk rs)
            (= x \<) (recur xs dp true rs)
            (= x \{) (recur xs (inc dp) sk rs)
            (= x \}) (recur xs (dec dp) sk (+ rs dp))
            :else    (recur xs dp sk rs)) rs)))

(defn garbage [xs]
  (loop [xs xs sk false rs 0]
    (if-let [[x & xs] (seq xs)]
      (cond (= x \!) (recur (rest xs) sk rs)
            (= x \>) (recur xs false rs)
            sk       (recur xs sk (inc rs))
            (= x \<) (recur xs true rs)
            :else    (recur xs sk rs)) rs)))

(let [in (first (line-seq (java.io.BufferedReader. *in*)))]
  (println "Part A:" (score in))
  (println "Part B:" (garbage in)))
