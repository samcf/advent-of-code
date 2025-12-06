(defn parse [s]
  (if-let [x (parse-long (str s))]
    x (case (str s) "+" '+ "*" '* nil)))

(defn parse-line-xf [xs]
  (into [] (map parse) xs))

(def parse-parts-xf
  (comp (map (fn [ln] (re-seq #"\w+|\+|\*" ln)))
        (map parse-line-xf)))

(defn join
  ([] 0)
  ([a] a)
  ([a b] (+ (* a 10) b)))

(defn solve [xs]
  (apply (resolve (first xs)) (rest xs)))

(defn solve' [xs]
  (let [acc (fn [row col] ((xs row) col))]
    (loop [idx (dec (count (first xs)))
           dig (list)
           sum 0]
      (if (< idx 0) sum
          (let [xf (comp (map (fn [row] (acc row idx))) (filter number?))
                ds (transduce xf join (range 4))
                f  (acc 4 idx)]
            (if (symbol? f)
              (recur (- idx 2) (list) (+ sum (apply (resolve f) (conj dig ds))))
              (recur (- idx 1) (conj dig ds) sum)))))))

(let [in (line-seq (java.io.BufferedReader. *in*))
      xs (apply mapv vector (into [] parse-parts-xf in))
      ys (into [] (map parse-line-xf) in)]
  (println "Part A:" (transduce (comp (map reverse) (map solve)) + xs))
  (println "Part B:" (solve' ys)))
