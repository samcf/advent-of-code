(require '[clojure.math :refer [sqrt pow]]
         '[clojure.math.combinatorics :refer [combinations]])

(def queue clojure.lang.PersistentQueue/EMPTY)
(def into' (fnil into (hash-set)))

(defn distance [[ax ay az] [bx by bz]]
  (sqrt (+ (pow (- ax bx) 2)
           (pow (- ay by) 2)
           (pow (- az bz) 2))))

(defn reachable [m k]
  (loop [next (conj queue k) vstd (hash-set)]
    (if (seq next)
      (let [k (peek next)]
        (if (not (vstd k))
          (recur (into (pop next) (m k)) (conj vstd k))
          (recur (pop next) vstd))) vstd)))

(defn circuits [m]
  (loop [m m rs []]
    (if (seq m)
      (let [ks (reachable m (key (first m)))]
        (recur (apply dissoc m ks) (conj rs ks)))
      rs)))

(defn connect [t]
  (fn [m [_ a b]]
    (if (contains? (reachable m a) b) m
        (let [m (merge-with into' m {a #{b} b #{a}})]
          (if (= (+ (count (circuits m)) (- t (count m))) 1)
            (reduced [a b]) m)))))

(defn solve-a [m]
  (->> (circuits m) (map count) (sort >) (take 3) (reduce *)))

(defn solve-b [[[ax _] [bx _]]]
  (* ax bx))

(let [in (line-seq (java.io.BufferedReader. *in*))
      rf (connect (count in))
      xf (comp
          (map (fn [ln] (re-seq #"\d+" ln)))
          (map (fn [xs] (into [] (map parse-long) xs))))
      xs (sort
          (map
           (fn [[a b]] [(distance a b) a b])
           (combinations (sequence xf in) 2)))]
  (println "Part A:" (solve-a (reduce rf {} (take 1000 xs))))
  (println "Part B:" (solve-b (reduce rf {} xs))))
