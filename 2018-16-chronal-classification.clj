(defn parse [in]
  (let [xf (comp (mapcat (fn [ln] (re-seq #"\d+" ln))) (map parse-long))]
    (loop [xs in ys [] zs [] flag false]
      (if (seq xs)
        (if (= (first xs) "")
          (recur (rest xs) ys zs true)
          (if (false? flag)
            (recur (drop 4 xs) (into ys xf (take 3 xs)) zs flag)
            (recur (rest xs) ys (into zs xf (take 1 xs)) flag)))
        [(into [] (comp (partition-all 4) (partition-all 3)) ys)
         (into [] (partition-all 4) zs)]))))

(def instructions
  {'addr (fn [a b] (+ (a (b 1)) (a (b 2))))
   'addi (fn [a b] (+ (a (b 1)) (b 2)))
   'mulr (fn [a b] (* (a (b 1)) (a (b 2))))
   'muli (fn [a b] (* (a (b 1)) (b 2)))
   'banr (fn [a b] (bit-and (a (b 1)) (a (b 2))))
   'bani (fn [a b] (bit-and (a (b 1)) (b 2)))
   'borr (fn [a b] (bit-or (a (b 1)) (a (b 2))))
   'bori (fn [a b] (bit-or (a (b 1)) (b 2)))
   'setr (fn [a b] (a (b 1)))
   'seti (fn [_ b] (b 1))
   'gtir (fn [a b] (if (> (b 1) (a (b 2))) 1 0))
   'gtri (fn [a b] (if (> (a (b 1)) (b 2)) 1 0))
   'gtrr (fn [a b] (if (> (a (b 1)) (a (b 2))) 1 0))
   'eqir (fn [a b] (if (= (b 1) (a (b 2))) 1 0))
   'eqri (fn [a b] (if (= (a (b 1)) (b 2)) 1 0))
   'eqrr (fn [a b] (if (= (a (b 1)) (a (b 2))) 1 0))})

(defn execute [mem ins f]
  (assoc mem (ins 3) (f mem ins)))

(defn matches [[src ins dst]]
  (into
   (hash-set)
   (comp
    (filter (fn [xf] (= (execute src ins (val xf)) dst)))
    (map key)) instructions))

(defn dict [xs]
  (let [xs (into {} (map (fn [x] [(first (second x)) (matches x)])) xs)]
    (loop [xs xs rs {}]
      (if (seq xs)
        (let [[k v] (first (filter (comp #{1} count val) xs))]
          (recur
           (dissoc (update-vals xs (fn [ys] (disj ys (first v)))) k)
           (assoc rs k (first v))))
        rs))))

(defn run [ins dict]
  (reduce
   (fn [mem ins]
     (let [f (instructions (dict (ins 0)))]
       (execute mem ins f))) [0 0 0 0] ins))

(let [in (line-seq (java.io.BufferedReader. *in*))
      xf (comp
          (map matches)
          (map count)
          (map (fn [x] (if (>= x 3) 1 0))))
      [xs ins] (parse in)]
  (println "Part A:" (transduce xf + xs))
  (println "Part B:" ((run ins (dict xs)) 0)))
