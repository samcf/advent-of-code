(def queue clojure.lang.PersistentQueue/EMPTY)
(def dirs [[0 -1] [1 0] [0 1] [-1 0]])

(defn space? [[x y] z]
  (and (not (or (neg? x) (neg? y)))
       (even?
        (Long/bitCount
         (+ (* x x) (* 3 x) (* 2 x y) (* y y) y z)))))

(defn add [[ax ay] [bx by]]
  [(+ ax bx) (+ ay by)])

(defn minimum-steps [xf src dst]
  (loop [queue (conj queue [src]) visit (hash-set)]
    (let [path (peek queue)
          node (peek path)]
      (cond (= node dst) (dec (count path))
            (visit node) (recur (pop queue) visit)
            :else        (recur (into (pop queue) (xf path) dirs) (conj visit node))))))

(defn unique-points [xf src lim]
  (loop [queue (conj queue [src]) visit (hash-set)]
    (if-let [path (peek queue)]
      (let [node (peek path)]
        (cond (= (dec (count path)) lim)
              (recur (pop queue) (conj visit node))
              (visit node)
              (recur (pop queue) visit)
              :else
              (recur (into (pop queue) (xf path) dirs) (conj visit node))))
      (count visit))))

(let [in (line-seq (java.io.BufferedReader. *in*))
      xn (parse-long (first in))
      xf (fn [path]
           (comp (map    (fn [off] (add (peek path) off)))
                 (filter (fn [pos] (space? pos xn)))
                 (map    (fn [pos] (conj path pos)))))]
  (println "Part A:" (minimum-steps xf [1 1] [31 39]))
  (println "Part B:" (unique-points xf [1 1] 50)))
