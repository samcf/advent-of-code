(def dirs [[1 1] [1 0] [1 -1] [0 1] [0 -1] [-1 1] [-1 0] [-1 -1]])

(defn index [cs x y]
  (+ (* y cs) x))

(defn neighbors [xs cs rs ds sx sy]
  (for [offv dirs
        step (range 1 (inc ds))
        :let [[ox oy] offv]
        :let [bx (+ sx (* step ox))
              by (+ sy (* step oy))]
        :while (and (< -1 bx cs) (< -1 by rs))
        :let [ax (+ sx (* (dec step) ox))
              ay (+ sy (* (dec step) oy))
              av (xs (index cs ax ay))
              bv (xs (index cs bx by))]
        :while (or (= step 1) (= av \.))
        :when  (or (= bv \L) (= bv \#))]
    bv))

(defn step-xf [cs rs ds th xs]
  (comp (map-indexed vector)
        (map (fn [[ix ch]] [(mod ix cs) (int (/ ix cs)) ch]))
        (map (fn [[sx sy ch]]
               (if (= ch \.) ch
                   (let [adj (neighbors xs cs rs ds sx sy)
                         frq (frequencies adj)]
                     (cond (and (= ch \L) (=  (get frq \# 0)  0)) \#
                           (and (= ch \#) (>= (get frq \# 0) th)) \L
                           :else ch)))))))

(defn run [xs xf]
  (loop [prev xs]
    (let [next (into [] (xf prev) prev)]
      (if (= prev next)
        ((frequencies next) \#)
        (recur next)))))

(let [in (line-seq (java.io.BufferedReader. *in*))
      cs (count (first in))
      rs (count in)
      vs (into [] cat in)]
  (println "Part A:" (run vs (partial step-xf cs rs 1 4)))
  (println "Part B:" (run vs (partial step-xf cs rs (max cs rs) 5))))
