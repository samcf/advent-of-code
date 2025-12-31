(require '[clojure.math.combinatorics :refer [cartesian-product combinations]])

(def dirs {1 [1] 2 [1 -1] 3 [1 -1] 4 [-1]})
(def queue clojure.lang.PersistentQueue/EMPTY)
(def match #"(\w+) generator|(\w+)-compatible microchip")

(defn parse [in]
  (->> (reduce-kv
        (fn [rs idx val]
          (reduce
           (fn [rs [_ a b]]
             (cond-> rs
               a (assoc-in [a 0] (inc idx))
               b (assoc-in [b 1] (inc idx)))) rs (re-seq match val))) {} (vec in))
       vals (map (comp vec vals)) sort vec))

(defn create-key [board]
  (update (dissoc board :steps) :pairs (comp vec sort)))

(defn valid? [board]
  (boolean
   (reduce
    (fn [s [a b]]
      (if (not= a b)
        (if (s b)     (reduced false) (conj s a (- b)))
        (if (s (- a)) (reduced false) (conj s a))))
    (hash-set) (:pairs board))))

(defn completed? [board]
  (every? #{[4 4]} (:pairs board)))

(defn paths [board]
  (reduce-kv
   (fn [res idx [a b]]
     (cond-> res
       (= (:floor board) a) (conj [idx 0])
       (= (:floor board) b) (conj [idx 1]))) []
   (:pairs board)))

(defn changes [board]
  (let [paths (paths board)]
    (cartesian-product
     (into (combinations paths 2) (combinations paths 1))
     (dirs (:floor board)))))

(defn change [board]
  (fn [[paths offset]]
    (-> (reduce
         (fn [board path]
           (update board :pairs update-in path + offset)) board paths)
        (update :floor + offset)
        (update :steps inc))))

(defn solve [board]
  (loop [queue (conj queue board) visit (hash-set)]
    (let [board (peek queue) key (create-key board)]
      (if (completed? board) (:steps board)
          (if (visit key) (recur (pop queue) visit)
              (let [xf (comp (map (change board)) (filter valid?))]
                (recur (into (pop queue) xf (changes board))
                       (conj visit key))))))))

(let [in (line-seq (java.io.BufferedReader. *in*))
      rs {:pairs (parse in) :floor 1 :steps 0}]
  (println "Part A:" (solve rs))
  (println "Part B:" (solve (update rs :pairs conj [1 1] [1 1]))))
