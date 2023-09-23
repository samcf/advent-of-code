(require '[clojure.set :refer [difference]])

(defn neighbors3 [v]
  (let [[ax ay az] v]
    (for [bx [-1 0 1] by [-1 0 1] bz [-1 0 1]
          :let  [b [(+ ax bx) (+ ay by) (+ az bz)]]
          :when (not= v b)] b)))

(defn neighbors4 [v]
  (let [[ax ay az ah] v]
    (for [bx [-1 0 1] by [-1 0 1] bz [-1 0 1] bh [-1 0 1]
          :let  [b [(+ ax bx) (+ ay by) (+ az bz) (+ ah bh)]]
          :when (not= v b)] b)))

(def neighbors
  (memoize
   (fn [v]
     (if (= (count v) 3)
       (neighbors3 v)
       (neighbors4 v)))))

(defn turn [xs [k v]]
  (let [xf (comp (map xs) (filter #{\#}))
        ct (count (take 4 (sequence xf (neighbors k))))]
    (cond (and (= v \#) (<= 2 ct 3)) \#
          (and (= v \.) (= ct 3))    \#
          :else \.)))

(defn step [xs]
  (let [nf (comp (filter (comp #{\#} val)) (map key) (mapcat neighbors)) ;; find all neighbors for all active cubes
        uk (difference (into #{} nf xs) (set (keys xs)))                 ;; find the difference from existing keys
        nk (sequence (map (juxt identity (constantly \.))) uk)           ;; turn these into key/value pairs
        xf (map (juxt first (partial turn xs)))]                         ;; turn existing cubes
    (into xs xf (into nk xs))))

(defn active [xs]
  (transduce (map (fn [[_ v]] (if (= v \#) 1 0))) + xs))

(let [in (line-seq (java.io.BufferedReader. *in*))
      xf (fn [n] (comp (map-indexed (fn [y r] (map-indexed (fn [x v] [(into [x y] (repeat n 0)) v]) r))) cat))]
  (println "Part A:" (active (nth (iterate step (into {} (xf 1) in)) 6)))
  (println "Part B:" (active (nth (iterate step (into {} (xf 2) in)) 6))))
