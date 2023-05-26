(defn parse [ln]
  (let [[_ c n] (re-find #"(\w){1}([0-9]+)" ln)]
    [(first c) (Integer. n)]))

(defn rotate [x y deg]
  (case deg
    90  [(* y -1) x]
    180 [(* x -1) (* y -1)]
    270 [y (* x -1)]))

(defn solve-a [[ax ay bx by] [cd vl]]
  (case cd
    \N [ax (- ay vl) bx by]
    \E [(+ ax vl) ay bx by]
    \S [ax (+ ay vl) bx by]
    \W [(- ax vl) ay bx by]
    \L (let [[rx ry] (rotate bx by (- 360 vl))] [ax ay rx ry])
    \R (let [[rx ry] (rotate bx by vl)] [ax ay rx ry])
    \F [(+ ax (* vl bx)) (+ ay (* vl by)) bx by]))

(defn solve-b [[ax ay bx by] [cd vl]]
  (case cd
    \N [ax ay bx (- by vl)]
    \E [ax ay (+ bx vl) by]
    \S [ax ay bx (+ by vl)]
    \W [ax ay (- bx vl) by]
    \L (let [[rx ry] (rotate bx by (- 360 vl))] [ax ay rx ry])
    \R (let [[rx ry] (rotate bx by vl)] [ax ay rx ry])
    \F [(+ ax (* vl bx)) (+ ay (* vl by)) bx by]))

(let [in (line-seq (java.io.BufferedReader. *in*))
      cf (fn [[x y]] (+ (abs x) (abs y)))
      rf (fn [f] (completing f cf))]
  (println "Part A:" (transduce (map parse) (rf solve-a) [0 0  1  0] in))
  (println "Part A:" (transduce (map parse) (rf solve-b) [0 0 10 -1] in)))
