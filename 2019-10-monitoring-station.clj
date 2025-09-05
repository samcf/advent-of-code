(def tau (* 2 Math/PI))

(defn parse [len]
  (comp
   (mapcat seq)
   (map-indexed vector)
   (map (fn [[i c]] [(list (mod i len) (quot i len)) c]))
   (filter (comp #{\#} second))
   (map first)))

(defn angle [[ax ay]]
  (fn [[bx by]]
    (let [r (Math/atan2 (- bx ax) (- ay by))]
      (if (< r 0) (+ tau r) r))))

(defn angles [xs a]
  (count (into (hash-set) (map (angle a)) (disj xs a))))

(defn asteroids [xs a]
  (let [f (angle a)]
    (apply merge-with into (map (fn [b] {(f b) [b]}) xs))))

(defn vaporized-at [rs n]
  (loop [rs rs ks (cycle (sort (keys rs))) nth 0 point nil]
    (if (not= nth n)
      (let [xs (rs (first ks))]
        (if (> (count xs) 1)
          (recur (update rs (first ks) rest) (rest ks) (inc nth) (first xs))
          (recur (dissoc rs (first ks))      (rest ks) (inc nth) (first xs)))) point)))

(let [in (line-seq (java.io.BufferedReader. *in*))
      xs (into (hash-set) (parse (count in)) in)
      rs (first
          (into
           (sorted-map-by >)
           (map (fn [a] [(angles xs a) a])) xs))]
  (println "Part A:" (key rs))
  (println
   "Part B:"
   (let [[x y] (vaporized-at (asteroids xs (val rs)) 200)]
     (+ (* x 100) y))))
