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

(defn offset [idx]
  (into
   (list)
   (map (fn [off] (+ idx off)))
   (list 2 1 0 -1 -2)))

(defn relevant [xs]
  (into (hash-set) (mapcat offset) xs))

(defn step [rs]
  (fn [xs]
    (let [f (fn [idx] (if (xs idx) \# \.))]
      (into
       (hash-set)
       (comp
        (map (juxt identity offset))
        (map (juxt first (comp (partial map f) second)))
        (filter (comp rs second))
        (map first))
       (relevant xs)))))

(defn solve [xs f t]
  (loop [xs xs x 0 a ##Inf b 0 c 0 d 0]
    (if (= (- a b) (- b c) (- c d))
      (+ (* (- t x) (- a b)) a)
      (let [xs (f xs)]
        (recur xs (inc x) (reduce + xs) a b c)))))

(let [[in _ & rs] (line-seq (java.io.BufferedReader. *in*))
      xs (into (hash-set) init-xf in)
      xf (step (into (hash-set) rules-xf rs))]
  (println "Part A:" (reduce + (nth (iterate xf xs) 20)))
  (println "Part B:" (solve xs xf 5e10)))
