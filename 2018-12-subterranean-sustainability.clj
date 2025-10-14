(def init-xf
  (comp
   (drop 15)
   (map-indexed vector)
   (filter (comp #{\#} second))
   (map first)))

(def rules-xf
  (comp
   (map (fn [ln] (re-seq #"#|\." ln)))
   (map (fn [xs] (map first xs)))
   (map (fn [xs] [(take 5 xs) (last xs)]))
   (filter (comp #{\#} second))
   (map first)))

(defn nearby [idx]
  (into
   (list)
   (map (fn [off] (+ idx off)))
   (list 2 1 0 -1 -2)))

(defn relevant [xs]
  (into (hash-set) (mapcat nearby) xs))

(defn advance [rs]
  (fn [xs]
    (let [f (fn [idx] (if (xs idx) \# \.))]
      (into
       (hash-set)
       (filter (comp rs (partial map f) nearby))
       (relevant xs)))))

(defn solve [f xs t]
  (loop [xs xs x 0 a ##Inf b 0 c 0 d 0]
    (if (= (- a b) (- b c) (- c d))
      (+ (* (- t x) (- a b)) a)
      (let [xs (f xs)]
        (recur xs (inc x) (reduce + xs) a b c)))))

(let [[in _ & rs] (line-seq (java.io.BufferedReader. *in*))
      xs (into (hash-set) init-xf in)
      xf (advance (into (hash-set) rules-xf rs))]
  (println "Part A:" (reduce + (nth (iterate xf xs) 20)))
  (println "Part B:" (solve xf xs 5e10)))
