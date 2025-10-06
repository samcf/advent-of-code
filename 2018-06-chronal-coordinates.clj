(def neighbors
  [{:x -1 :y 0} {:x 0 :y -1} {:x 1 :y 0} {:x 0 :y 1}])

(defn add [a b]
  {:x (+ (:x a) (:x b))
   :y (+ (:y a) (:y b))})

(defn div [a n]
  {:x (quot (:x a) n)
   :y (quot (:y a) n)})

(defn bounds [xs f]
  (reduce
   (fn [a b]
     {:x (f (:x a) (:x b))
      :y (f (:y a) (:y b))}) xs))

(defn manhattan [a b]
  (+ (abs (- (:x a) (:x b)))
     (abs (- (:y a) (:y b)))))

(defn area [pred boundary? start]
  (loop [queue (conj clojure.lang.PersistentQueue/EMPTY start)
         vstd (hash-set)
         area 0]
    (if (seq queue)
      (let [point (peek queue) queue (pop queue)]
        (cond (vstd point)       (recur queue vstd area)
              (not (pred point)) (recur queue (conj vstd point) area)
              (boundary? point)  0
              :else
              (recur (into queue (map (fn [uv] (add point uv))) neighbors)
                     (conj vstd point)
                     (inc area)))) area)))

(defn nearest [xs a]
  (let [[c d] (sort-by val (into {} (map (fn [b] {b (manhattan a b)})) xs))]
    (if (= (val c) (val d)) nil
        (key c))))

(defn area-nearest [xs f]
  (map (fn [a] (area (fn [b] (= (nearest xs b) a)) f a))))

(defn area-within [xs n]
  (fn [a] (< (transduce (map (fn [b] (manhattan a b))) + xs) n)))

(let [in (line-seq (java.io.BufferedReader. *in*))
      xf (comp (map (fn [ln] (re-seq #"\d+" ln)))
               (map (fn [[x y]] [(parse-long x) (parse-long y)]))
               (map (fn [[x y]] {:x x :y y})))
      xs (into [] xf in)
      ba (bounds xs min)
      bb (bounds xs max)
      bf (fn [a]
           (or (= (:x a) (:x ba))
               (= (:y a) (:y ba))
               (= (:x a) (:x bb))
               (= (:y a) (:y bb))))]
  (println "Part A:" (transduce (area-nearest xs bf) max 0 xs))
  (println "Part B:" (area (area-within xs 10000) bf
                           (div (reduce add xs) (count xs)))))
