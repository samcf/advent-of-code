(def neighbors
  [{:x -1 :y 0} {:x 0 :y -1} {:x 1 :y 0} {:x 0 :y 1}])

(defn add [a b]
  {:x (+ (:x a) (:x b)) :y (+ (:y a) (:y b))})

(defn div [a n]
  {:x (quot (:x a) n) :y (quot (:y a) n)})

(defn manhattan [a b]
  (+ (abs (- (:x a) (:x b)))
     (abs (- (:y a) (:y b)))))

(defn nearest [xs a]
  (let [[c d] (sort-by val (into {} (map (fn [b] {b (manhattan a b)})) xs))]
    (if (= (val c) (val d)) nil
        (key c))))

(defn area [pred limit point]
  (loop [queue (conj clojure.lang.PersistentQueue/EMPTY point)
         vstd  (hash-set)
         area  0]
    (if (> area limit) 0
        (if (not (seq queue)) area
            (let [next (peek queue) list (pop queue)]
              (if (vstd next)
                (recur list vstd area)
                (if (not (pred next))
                  (recur list (conj vstd next) area)
                  (recur (into list (map (fn [off] (add next off))) neighbors)
                         (conj vstd next)
                         (inc area)))))))))

(defn area-nearest [xs n]
  (map (fn [a] (area (fn [b] (= (nearest xs b) a)) n a))))

(defn area-within [xs n]
  (fn [a] (< (transduce (map (fn [b] (manhattan a b))) + xs) n)))

(let [in (line-seq (java.io.BufferedReader. *in*))
      xf (comp (map (fn [ln] (re-seq #"\d+" ln)))
               (map (fn [[x y]] [(parse-long x) (parse-long y)]))
               (map (fn [[x y]] {:x x :y y})))
      xs (into [] xf in)]
  (println "Part A:" (transduce (area-nearest xs 5000) max 0 xs))
  (println "Part B:" (area (area-within xs 10000) ##Inf
                           (div (reduce add xs) (count xs)))))
