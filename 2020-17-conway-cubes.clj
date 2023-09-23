(defn parse [n]
  (comp (map-indexed (fn [y r] (map-indexed (fn [x v] [(into [x y] (repeat n 0)) v]) r))) cat
        (filter (comp #{\#} second))))

(defn neighbors3 [from]
  (let [[ax ay az] from]
    (for [bx [-1 0 1]
          by [-1 0 1]
          bz [-1 0 1]
          :let  [next [(+ ax bx) (+ ay by) (+ az bz)]]
          :when (not= from next)]
      next)))

(defn neighbors4 [from]
  (let [[ax ay az ah] from]
    (for [bx [-1 0 1]
          by [-1 0 1]
          bz [-1 0 1]
          bh [-1 0 1]
          :let  [next [(+ ax bx) (+ ay by) (+ az bz) (+ ah bh)]]
          :when (not= from next)]
      next)))

(def neighbors
  (memoize
   (fn [from]
     (if (= (count from) 3)
       (neighbors3 from)
       (neighbors4 from)))))

(defn turn [xs [k v]]
  (let [xf (comp (map xs) (filter #{\#}) (take 4))
        ct (count (sequence xf (neighbors k)))]
    (cond (and (= v \#) (<= 2 ct 3)) \#
          (and (= v \.) (= ct 3))    \#
          :else \.)))

(defn step [xs]
  (let [vs (vec xs)
        ks (set (keys xs))
        nf (comp (mapcat (comp neighbors key))
                 (distinct)
                 (filter (complement ks))
                 (map (juxt identity (constantly \.))))
        xf (comp (map (juxt first (partial turn xs)))
                 (filter (comp #{\#} second)))]
    (into {} xf (into vs nf vs))))

(let [in (line-seq (java.io.BufferedReader. *in*))]
  (println "Part A:" (count (nth (iterate step (into {} (parse 1) in)) 6)))
  (println "Part B:" (count (nth (iterate step (into {} (parse 2) in)) 6))))
