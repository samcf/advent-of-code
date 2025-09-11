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

(def zero [0 0 0 0])
(def moons (range 4))
(def duals (pairs (range 4)))

(defn velocity-rf [xs [i j]]
  (let [a (xs i) b (+ i 4) c (xs j) d (+ j 4)]
    (cond
      (= a c) xs
      (> a c) (assoc xs b (dec (xs b)) d (inc (xs d)))
      (< a c) (assoc xs b (inc (xs b)) d (dec (xs d))))))

(defn velocity [xs]
  (reduce velocity-rf xs duals))

(defn position [xs]
  (assoc
   xs
   0 (+ (xs 0) (xs 4)) 1 (+ (xs 1) (xs 5))
   2 (+ (xs 2) (xs 6)) 3 (+ (xs 3) (xs 7))))

(def step (comp position velocity))

(defn simulate [f [xs ys zs] num]
  (loop [xs xs ys ys zs zs cur 0]
    (if (not= cur num)
      (recur (f xs) (f ys) (f zs) (inc cur))
      [xs ys zs])))

(defn energy [[xs ys zs]]
  (transduce
   (comp
    (map (fn [idx] [abs idx (+ idx 4)]))
    (map
     (fn [[f a b]]
       (* (+ (f (xs a)) (f (ys a)) (f (zs a)))
          (+ (f (xs b)) (f (ys b)) (f (zs b))))))) + moons))

(defn period [f xs]
  (loop [xs xs i 1]
    (let [xs (f xs)]
      (if (= (subvec xs 4) zero)
        i (recur xs (inc i))))))

(let [in (line-seq (java.io.BufferedReader. *in*))
      re (re-seq #"-?\d+" (apply str in))
      xf (comp (map parse-long) (partition-all 3))
      xs (sequence
          (map (fn [xs] (into xs zero)))
          (transpose (sequence xf re)))]
  (println "Part A:" (energy (simulate step xs 1000)))
  (println "Part B:" (transduce (map (fn [xs] (period step xs))) lcm 1 xs)))
