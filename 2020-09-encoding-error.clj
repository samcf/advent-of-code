(defn valid? [[x xs]]
  (some (fn [n] ((disj xs n) (- x n))) xs))

(defn parts [n xs]
  (map vector (drop n xs) (map set (partition n 1 xs))))

(defn search [xs n]
  (loop [xs xs ys xs zs []]
    (let [s (reduce + zs)]
      (cond (= s n) zs
            (> s n) (recur (rest xs) (rest xs) [])
            :else   (recur xs        (rest ys) (conj zs (first ys)))))))

(let [in (line-seq (java.io.BufferedReader. *in*))
      xs (map (fn [x] (Integer. x)) in)
      vx (ffirst (filter (complement valid?) (parts 25 xs)))]
  (println "Part A:" vx)
  (println "Part B:" (->> (search xs vx) (into (sorted-set)) ((juxt first last)) (apply +))))
