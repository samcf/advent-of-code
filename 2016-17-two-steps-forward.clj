(import '[java.security MessageDigest])

(def queue clojure.lang.PersistentQueue/EMPTY)
(def instance (MessageDigest/getInstance "MD5"))
(def direction {[0 -1] \U [0  1] \D [-1 0] \L [1  0] \R})
(def unlocked? #{\b \c \d \e \f})

(defn encode [s]
  (->> (.getBytes s "UTF-8") (.digest instance) (BigInteger. 1) (format "%032x")))

(defn add [[ax ay] [bx by]]
  [(+ ax bx) (+ ay by)])

(defn sub [[[ax ay] [bx by]]]
  [(- bx ax) (- by ay)])

(defn within? [[x y]]
  (and (>= x 0) (>= y 0) (< x 4) (< y 4)))

(defn path [xs]
  (transduce
   (map (comp direction sub)) str
   (partition 2 1 xs)))

(defn neighbors [s]
  (cond-> []
    (unlocked? (.charAt s 0)) (conj [0 -1])
    (unlocked? (.charAt s 1)) (conj [0  1])
    (unlocked? (.charAt s 2)) (conj [-1 0])
    (unlocked? (.charAt s 3)) (conj [1  0])))

(defn advance [path]
  (keep
   (fn [off]
     (let [vec (add (peek path) off)]
       (when (within? vec)
         (conj path vec))))))

(defn shortest-path [seed]
  (loop [queue (conj queue [[0 0]])]
    (let [curr (peek queue)]
      (if (= (peek curr) [3 3])
        (path curr)
        (let [xs (neighbors (encode (str seed (path curr))))]
          (recur (into (pop queue) (advance curr) xs)))))))

(defn longest-path [seed]
  (loop [queue (conj queue [[0 0]]) prev 0]
    (if-let [curr (peek queue)]
      (if (= (peek curr) [3 3])
        (recur (pop queue) curr)
        (let [xs (neighbors (encode (str seed (path curr))))]
          (recur (into (pop queue) (advance curr) xs) prev)))
      (count (path prev)))))

(let [in (first (line-seq (java.io.BufferedReader. *in*)))]
  (println "Part A:" (shortest-path in))
  (println "Part B:" (longest-path  in)))
