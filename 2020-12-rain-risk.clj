(defn parse [ln]
  (let [[_ c n] (re-find #"(\w){1}([0-9]+)" ln)]
    [(first c) (Integer. n)]))

(def heading {0 [1 0] 90 [0 -1] 180 [-1 0] 270 [0 1] 360 [1 0]})

(defn solve-a [[ax ay hd] [cd mv]]
  (case cd
    \N [ax (- ay mv) hd]
    \E [(+ ax mv) ay hd]
    \S [ax (+ ay mv) hd]
    \W [(- ax mv) ay hd]
    \L [ax ay (mod (+ hd mv) 360)]
    \R [ax ay (mod (- hd mv) 360)]
    \F (let [[ux uy] (heading hd)]
         [(+ ax (* mv ux)) (+ ay (* mv uy)) hd])))

(defn rotate [x y deg dir]
  (case [dir deg]
    ([1 90] [-1 270])  [(* y -1) x]
    ([1 180] [-1 180]) [(* x -1) (* y -1)]
    ([1 270] [-1 90])  [y (* x -1)]))

(defn solve-b [[ax ay bx by] [cd mv]]
  (case cd
    \N [ax ay bx (- by mv)]
    \E [ax ay (+ bx mv) by]
    \S [ax ay bx (+ by mv)]
    \W [ax ay (- bx mv) by]
    \L (let [[rx ry] (rotate bx by mv -1)] [ax ay rx ry])
    \R (let [[rx ry] (rotate bx by mv  1)] [ax ay rx ry])
    \F [(+ ax (* mv bx)) (+ ay (* mv by)) bx by]))

(let [in (line-seq (java.io.BufferedReader. *in*))
      cf (fn [[x y]] (+ (abs x) (abs y)))
      rf (fn [f] (completing f cf))]
  (println "Part A:" (transduce (map parse) (rf solve-a) [0 0 0] in))
  (println "Part A:" (transduce (map parse) (rf solve-b) [0 0 10 -1] in)))
