(defn parse [s]
  (let [n (parse-long (subs s 1))]
    (case (first s)
      \L [-1 n]
      \R [+1 n])))

(def rotate
  {0 [0 -1] 1 [1 0] 2 [0 1] 3 [-1 0]})

(defn add [[ax ay] [bx by]]
  [(+ ax bx) (+ ay by)])

(defn mul [[ax ay] n]
  [(* ax n) (* ay n)])

(defn distance [[ax ay]]
  (+ (abs ax) (abs ay)))

(defn repeated [xs]
  (reduce
   (fn [s x] (if (s x) (reduced x) (conj s x)))
   (hash-set) xs))

(defn walk
  ([] {:pos [[0 0]] :dir 0})
  ([{:keys [pos]}] pos)
  ([{:keys [pos dir] :as m} [rot len]]
   (let [dir (mod (+ dir rot) 4)
         off (rotate dir)
         xfr (comp
              (map (fn [len] (mul off len)))
              (map (fn [off] (add (peek pos) off))))]
     (update (assoc m :dir dir) :pos into xfr (range 1 (inc len))))))

(let [in (first (line-seq (java.io.BufferedReader. *in*)))
      re (re-seq #"[L|R]\d+" in)
      xs (transduce (map parse) walk re)]
  (println "Part A:" (distance (peek xs)))
  (println "Part B:" (distance (repeated xs))))
