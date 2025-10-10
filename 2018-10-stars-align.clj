(defn bounds [xs]
  (loop [xs xs ax ##Inf ay ##Inf bx ##-Inf by ##-Inf]
    (if (seq xs)
      (let [point (first xs)]
        (recur
         (rest xs)
         (min (:ax point) ax)
         (min (:ay point) ay)
         (max (:ax point) bx)
         (max (:ay point) by)))
      {:ax ax :ay ay :bx bx :by by})))

(defn area [rect]
  (* (- (:bx rect) (:ax rect))
     (- (:by rect) (:ay rect))))

(defn step [{:keys [bx by] :as point}]
  (update (update point :ax + bx) :ay + by))

(defn wait [xs]
  (loop [prev xs min ##Inf time 0]
    (let [next (into [] (map step) prev)
          area (area (bounds next))]
      (if (< area min)
        (recur next area (inc time))
        {:stars prev :time time}))))

(defn draw [vs]
  (let [rect (bounds vs)
        exst (into (hash-set) (map (juxt :ax :ay)) vs)
        rows (range (:ay rect) (inc (:by rect)))
        cols (range (:ax rect) (inc (:bx rect)))]
    (map (fn [y] (map (fn [x] (if (exst [x y]) \█ \░))  cols)) rows)))

(let [in (line-seq (java.io.BufferedReader. *in*))
      xf (comp (map (fn [ln] (re-seq #"-?\d+" ln)))
               (map (fn [xs] (sequence (map parse-long) xs)))
               (map (fn [xs] (zipmap [:ax :ay :bx :by] xs))))
      xs (wait (into [] xf in))]
  (println "Part A:")
  (doseq [line (draw (:stars xs))]
    (println (apply str line)))
  (println "Part B:" (:time xs)))
