(defn gcd [a b]
  (if (> b 0)
    (recur b (mod a b)) a))

(defn lcm
  ([a] (* a 2))
  ([a b]
   (if (and (> a 0) (> b 0))
     (/ (* a b) (gcd a b)) 0)))

(defn transpose [xs]
  (apply mapv vector xs))

(defn pairs [xs]
  (lazy-seq
   (when-let [[x & xs] (seq xs)]
     (concat
      (map (fn [y] [x y]) xs)
      (pairs xs)))))

(def moons (range 4))
(def duals (pairs (range 4)))

(defn velocity [xs]
  (reduce
   (fn [xs [i j]]
     (let [a (xs i) b (xs j)]
       (cond
         (> a b) (update (update xs (+ i 4) dec) (+ j 4) inc)
         (< a b) (update (update xs (+ i 4) inc) (+ j 4) dec)
         (= a b) xs))) xs duals))

(defn position [xs]
  (assoc
   xs
   0 (+ (xs 0) (xs 4)) 1 (+ (xs 1) (xs 5))
   2 (+ (xs 2) (xs 6)) 3 (+ (xs 3) (xs 7))))

(def step (comp position velocity))

(defn simulate [[xs ys zs] num]
  (loop [xs xs ys ys zs zs cur 0]
    (if (not= cur num)
      (recur (step xs) (step ys) (step zs) (inc cur))
      [xs ys zs])))

(defn energy [[xs ys zs]]
  (transduce
   (comp
    (map (fn [idx] [abs idx (+ idx 4)]))
    (map
     (fn [[f a b]]
       (* (+ (f (xs a)) (f (ys a)) (f (zs a)))
          (+ (f (xs b)) (f (ys b)) (f (zs b))))))) + moons))

(defn period [xs]
  (loop [xs xs cur 1]
    (let [xs (step xs)]
      (if (not= 0 (xs 4) (xs 5) (xs 6) (xs 7))
        (recur xs (inc cur)) cur))))

(let [in (line-seq (java.io.BufferedReader. *in*))
      re (re-seq #"-?\d+" (apply str in))
      xf (comp (map parse-long) (partition-all 3))
      xs (transpose
          (concat
           (sequence xf re)
           (repeat 4 (repeat 4 0))))]
  (println "Part A:" (energy (simulate xs 1000)))
  (println "Part B:" (transduce (map period) lcm 1 xs)))
