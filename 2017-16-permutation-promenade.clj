(require '[clojure.string :refer [split]])

(def dancers
  {\a 0  \b 1  \c 2  \d 3
   \e 4  \f 5  \g 6  \h 7
   \i 8  \j 9  \k 10 \l 11
   \m 12 \n 13 \o 14 \p 15})

(defn spin [m n]
  (update-vals m (fn [i] (mod (+ i n) 16))))

(defn exch [m i j]
  (let [a (some (fn [r] (when (= (val r) i) (key r))) m)
        b (some (fn [r] (when (= (val r) j) (key r))) m)]
    (assoc m a j b i)))

(defn part [m a b]
  (assoc m a (m b) b (m a)))

(defn parse [s]
  (condp re-find s
    #"s(\d+)"            :>> (fn [[_   x]] [spin (parse-long x)])
    #"x(\d+)\/(\d+)"     :>> (fn [[_ a b]] [exch (parse-long a) (parse-long b)])
    #"p([a-z])\/([a-z])" :>> (fn [[_ a b]] [part (first a) (first b)])))

(defn dance
  ([] [])
  ([xs x] (conj xs x))
  ([xs]
   (fn [m]
     (reduce (fn [m [f & args]] (apply f m args)) m xs))))

(defn encode [m]
  (apply str (keys (sort-by val m))))

(defn dance-for [f m n]
  (loop [r (f m) t 1]
    (if (= m r)
      (nth (iterate f r) (rem n t))
      (recur (f r) (inc t)))))

(let [in (first (line-seq (java.io.BufferedReader. *in*)))
      f  (transduce (map parse) dance (split in #","))]
  (println "Part A:" (encode (f dancers)))
  (println "Part B:" (encode (dance-for f (f dancers) (dec 1e9)))))
