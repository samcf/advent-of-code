(defn gcd [a b]
  (if (zero? b)
    [a 1 0]
    (let [[g x y] (gcd b (mod a b))]
      [g y (- x (* y (quot a b)))])))

(defn crt [[a b] [c d]]
  (let [[g x _] (gcd b d) t (- c a)]
    (when (zero? (mod t g))
      (let [lcm (quot (* b d) g)
            sft (mod (quot (* t x) g) (quot d g))
            off (mod (+ a (* b sft)) lcm)]
        [off lcm]))))

(let [in (line-seq (java.io.BufferedReader. *in*))
      xf (comp (map (fn [ln] (re-seq #"\d+" ln)))
               (map (fn [xs] (map parse-long xs)))
               (map (fn [[a b _ c]] [(- b c a) b])))
      xs (into [] xf in)]
  (println "Part A:" (first (reduce crt xs)))
  (println "Part B:" (first (reduce crt (conj xs [4 11])))))
