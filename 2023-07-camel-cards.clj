(require '[clojure.string :refer [split replace]])

(def score {\* 1 \2 2 \3 3 \4 4 \5 5 \6 6 \7 7 \8 8 \9 9 \T 10 \J 11 \Q 12 \K 13 \A 14})

(declare possible)

(defn kind [hand]
  (let [xs (frequencies hand) vs (count xs)]
    (if-let [x (xs \*)]
      (first (possible (replace hand #"\*" "") x))
      (condp some (set (vals xs))
        #{5} 6
        #{4} 5
        #{3} (case vs 2 4 3 3)
        #{2} (case vs 3 2 4 1)
        0))))

(defn possible [s x]
  (let [ss (sorted-set-by >)
        xs (keys (dissoc score \J \*))]
    (case x
      1 (into ss (for [a xs] (kind (str s a))))
      2 (into ss (for [a xs b xs] (kind (str s a b))))
      3 (into ss (for [a xs b xs c xs] (kind (str s a b c))))
      4 (into ss (for [a xs b xs c xs d xs] (kind (str s a b c d))))
      5 #{6})))

(defn order [[a _] [b _]]
  (let [ax (kind a) bx (kind b)]
    (if (not= ax bx)
      (- ax bx)
      (loop [[a & as] a [b & bs] b]
        (if (not= a b)
          (- (score a) (score b))
          (recur as bs))))))

(defn reward [r [_ bid]]
  (* bid (inc r)))

(def parse-xf
  (comp (map (fn [s] (split s #" ")))
        (map (fn [[s x]] [s (parse-long x)]))))

(def replace-jokers-xf
  (comp (map (fn [s] (replace s #"J" "*")))))

(let [in (line-seq (java.io.BufferedReader. *in*))
      xs (sort order (sequence parse-xf in))
      ys (sort order (sequence (comp replace-jokers-xf parse-xf) in))]
  (println "Part A:" (transduce (map-indexed reward) + xs))
  (println "Part B:" (transduce (map-indexed reward) + ys)))
