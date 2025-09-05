(def tau (* 2 Math/PI))

(defn parse [len]
  (comp
   (mapcat seq)
   (map-indexed vector)
   (map (fn [[i c]] [(list (mod i len) (quot i len)) c]))
   (filter (comp #{\#} second))
   (map first)))

(defn angle [[ax ay] [bx by]]
  (let [r (Math/atan2 (- bx ax) (- ay by))]
    (if (< r 0) (+ tau r) r)))

(defn distance [[ax ay] [bx by]]
  (max (abs (- bx ax)) (abs (- by ay))))

(defn comp-fn [a]
  (fn [b c]
    (compare
     (distance a b)
     (distance a c))))

(defn asteroids [xs a]
  (let [cmp (comp-fn a)]
    (reduce
     (fn [m x] (update m (angle a x) (fnil conj (sorted-set-by cmp)) x))
     (sorted-map-by <)
     (disj xs a))))

(defn vaporized-at [rs n]
  (loop [rs rs ks (cycle (keys rs)) nth 0 point nil]
    (if (not= nth n)
      (let [xs (rs (first ks))]
        (if (> (count xs) 1)
          (recur (update rs (first ks) rest) (rest ks) (inc nth) (first xs))
          (recur (dissoc rs (first ks))      (rest ks) (inc nth) (first xs))))
      point)))

(let [in (line-seq (java.io.BufferedReader. *in*))
      xs (into #{} (parse (count in)) in)
      rs (into  {} (map (fn [a] [a (asteroids xs a)])) xs)
      kv (first
          (into
           (sorted-map-by >)
           (map (juxt (comp count val) key)) rs))]
  (println "Part A:" (key kv))
  (println
   "Part B:"
   (let [[x y] (vaporized-at (rs (val kv)) 200)]
     (+ (* x 100) y))))
