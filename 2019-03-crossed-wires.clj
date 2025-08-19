(defn parse [s]
  (mapcat (fn [[_ d x]] [(first d) (parse-long x)])
          (re-seq #"([A-Z])(\d+)" s)))

(defn path [xs]
  (loop [xs xs rs (list 0 0 0)]
    (if (seq xs)
      (let [[x y s] rs
            [d n] xs
            xs (rest (rest xs))
            st (+ s n)]
        (case d
          \U (recur xs (conj rs st (+ y n) x))
          \R (recur xs (conj rs st y (+ x n)))
          \D (recur xs (conj rs st (- y n) x))
          \L (recur xs (conj rs st y (- x n)))))
      rs)))

(defn segments [xs]
  (partition 2 1 (partition 3 xs)))

(defn intersects-at
  [[[ax ay] [bx by]]
   [[cx cy] [dx dy]]]
  (cond (and (= ax bx) (= cx dx)) nil
        (and (= ay by) (= cy dy)) nil
        (and (= ax bx) (= cy dy)
             (< (min cx dx) ax (max cx dx))
             (< (min ay by) cy (max ay by))) [ax cy]
        (and (= ay by) (= cx dx)
             (< (min cy dy) ay (max cy dy))
             (< (min ax bx) cx (max ax bx))) [cx ay]
        :else nil))

(defn min-score-by [score-fn [path-a path-b]]
  (->> (for [seg-a path-a
             seg-b path-b
             :let [point (intersects-at seg-a seg-b)]
             :when point]
         (score-fn seg-a seg-b point))
       (apply min)))

(let [in (line-seq (java.io.BufferedReader. *in*))
      xs (map (comp segments path parse) in)]
  (println "Part A:" (min-score-by (fn [_ _ [x y]] (+ (abs x) (abs y))) xs))
  (println "Part B:" (min-score-by
                      (fn [[_ [ax ay an]] [_ [bx by bn]] [cx cy]]
                        (+ an bn
                           (abs (- cx ax))
                           (abs (- cy ay))
                           (abs (- cx bx))
                           (abs (- cy by)))) xs)))
