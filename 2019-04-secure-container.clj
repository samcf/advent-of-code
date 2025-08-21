(defn join [xs]
  (reduce (fn [x d] (+ (* x 10) d)) 0 xs))

(defn split [x]
  (loop [x x rs (list)]
    (if (> x 0) (recur (quot x 10) (conj rs (mod x 10)))
        rs)))

(defn skip [x]
  (loop [xs (split x) rs [] y 0 m nil]
    (if (seq xs)
      (let [[x & xs] xs]
        (cond (some? m) (recur xs (conj rs m) y m)
              (< x y)   (recur xs (conj rs y) x y)
              :else     (recur xs (conj rs x) x nil)))
      (join rs))))

(defn codes [start end]
  (lazy-seq
   (let [curr (skip start)]
     (when (<= curr end)
       (cons curr (codes (inc curr) end))))))

(defn valid-a [x]
  (true?
   (reduce
    (fn [x y] (if (= x y) (reduced true) y))
    (split x))))

(defn valid-b [x]
  (loop [xs (split x) match 0]
    (if (and (seq xs) (seq (rest xs)))
      (let [[x y] xs]
        (cond (and (= match 0) (= x y)) (recur (rest xs) 1)
              (and (= match 1) (= x y)) (recur (rest xs) 2)
              (and (= match 1) (not= x y)) true
              (and (= match 2) (not= x y)) (recur (rest xs) 0)
              :else (recur (rest xs) match)))
      (= match 1))))

(let [in (first (line-seq (java.io.BufferedReader. *in*)))
      rn (re-seq #"\d+" in)
      xf (fn [f] (map (fn [x] (if (f x) 1 0))))
      xs (codes (parse-long (first rn)) (parse-long (second rn)))]
  (println "Part A:" (transduce (xf valid-a) + xs))
  (println "Part B:" (transduce (xf valid-b) + xs)))
