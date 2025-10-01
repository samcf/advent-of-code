(require '[clojure.string :refer [replace]])

(def parse
  (comp
   (map (fn [ln] (replace ln #"falls|wakes" {"falls" "0" "wakes" "1"})))
   (map (fn [ln] (into [] (map parse-long) (re-seq #"\d+" ln))))))

(defn order [a b]
  (compare (subvec a 0 5) (subvec b 0 5)))

(defn expand [xs]
  (loop [xs xs id 0 pv 0 rs {}]
    (if (seq xs)
      (let [[[_ _ _ _ mn ev] & xs] xs]
        (case ev
          0 (recur xs id mn rs)
          1 (recur
             xs id pv
             (let [ms (map (fn [x] {x 1}) (range pv mn))]
               (update rs id (fn [m] (apply merge-with + m ms)))))
          (recur xs ev pv rs))) rs)))

(defn sum [xs] (reduce +   xs))
(defn zen [xs] (reduce max xs))
(defn top [xs] (key (first (sort-by val > xs))))

(defn strategy-a [rs]
  (let [x (top (update-vals rs (comp sum vals)))
        y (top (rs x))]
    (* x y)))

(defn strategy-b [rs]
  (let [x (top (update-vals rs (comp zen vals)))
        y (key (apply max-key val (rs x)))]
    (* x y)))

(let [in (line-seq (java.io.BufferedReader. *in*))
      xs (expand (sort order (into [] parse in)))]
  (println "Part A:" (strategy-a xs))
  (println "Part B:" (strategy-b xs)))
