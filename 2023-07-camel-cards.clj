(require '[clojure.string :refer [split replace]])

(def score {\* 1 \2 2 \3 3 \4 4 \5 5 \6 6 \7 7 \8 8 \9 9 \T 10 \J 11 \Q 12 \K 13 \A 14})
(def kinds {[1 1 1 1 1] 0 [1 1 1 2] 1 [1 2 2] 2 [1 1 3] 3 [2 3] 4 [1 4] 5 [5] 6})

(defn rewrite [xs x]
  (let [kv (first (sort-by val > xs))]
    (->> (assoc xs (key kv) (+ (val kv) x))
         (mapcat (fn [kv] (repeat (val kv) (key kv))))
         (apply str))))

(defn kind [hand]
  (let [xs (frequencies hand)]
    (if-let [x (xs \*)]
      (if (not= x 5) (recur (rewrite (dissoc xs \*) x)) 6)
      (kinds (sort (vals xs))))))

(defn order [[a _] [b _]]
  (let [ax (kind a) bx (kind b)]
    (if (not= ax bx)
      (- ax bx)
      (loop [[a & as] a [b & bs] b]
        (if (not= a b)
          (- (score a) (score b))
          (recur as bs))))))

(defn reward [rank [_ bid]]
  (* bid (inc rank)))

(def parse-xf
  (comp (map (fn [s] (split s #" ")))
        (map (fn [[s x]] [s (parse-long x)]))))

(def replace-xf
  (comp (map (fn [s] (replace s #"J" "*")))))

(let [in (line-seq (java.io.BufferedReader. *in*))
      xs (sort order (sequence parse-xf in))
      ys (sort order (sequence (comp replace-xf parse-xf) in))]
  (println "Part A:" (transduce (map-indexed reward) + xs))
  (println "Part B:" (transduce (map-indexed reward) + ys)))
