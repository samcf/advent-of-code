(require '[clojure.math :refer [ceil]])

(def parse
  (comp (map (fn [ln] (re-seq #"(\d+) (\w+)" ln)))
        (map (fn [xs] (mapcat (fn [[_ x k]] [(parse-long x) k]) xs)))
        (map reverse)
        (map (fn [xs] (partition 2 xs)))
        (map (fn [[[k v] & xs]] [k (conj xs v)]))))

(defn min-cost [m k n]
  (loop [xs {k n} su {} rs 0]
    (if-let [[k n] (first (seq xs))]
      (if (= k "ORE")
        (recur (dissoc xs k) su (+ rs n))
        (let [c (m k)
              p (first c)             ;; amount created per run
              s (get su k 0)          ;; amount available as surplus
              a (max (- n s) 0)       ;; amount needed after surplus
              l (max (- s n) 0)       ;; surplus leftover
              t (long (ceil (/ a p))) ;; multiplier
              w (- (* p t) a)         ;; surplus created
              ]
          (recur (merge-with
                  +
                  (dissoc xs k)
                  (into {} (map (fn [[c r]] [c (* r t)])) (rest c)))
                 (assoc su k (+ l w))
                 rs)))
      rs)))

(defn max-fuel [m n]
  (let [f (fn [x] (min-cost m "FUEL" x))]
    (loop [a 0 b n]
      (if (= a b) a
          (let [c (long (quot (inc (+ a b)) 2))]
            (if (<= (f c) n)
              (recur c b)
              (recur a (dec c))))))))

(let [in (line-seq (java.io.BufferedReader. *in*))
      xs (into {} parse in)]
  (println "Part A:" (min-cost xs "FUEL" 1))
  (println "Part B:" (max-fuel xs 1e12)))
