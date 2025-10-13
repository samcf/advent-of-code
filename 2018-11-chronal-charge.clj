(require '[clojure.string :refer [join]])

(defn cell [n]
  (fn [x y]
    (let [a (+ x 10)]
      (-> (* a y) (+ n) (* a) (quot 100) (mod 10) (- 5)))))

(defn sums [f]
  (loop [x 1 y 1 s {}]
    (if (> y 300) s
        (if (> x 300)
          (recur 1 (inc y) s)
          (let [p (f x y)
                a (get s [(dec x) y] 0)
                b (get s [x (dec y)] 0)
                c (get s [(dec x) (dec y)] 0)
                t (- (+ a b p) c)]
            (recur (inc x) y (assoc s [x y] t)))))))

(defn search [s j k]
  (loop [ax 1 ay 1 sz (dec j) max (first {[0 0 0] 0})]
    (let [bx (+ ax sz) by (+ ay sz)]
      (if (= sz k) (key max)
          (if (> by 300)
            (recur 1 1 (inc sz) max)
            (if (> bx 300)
              (recur 1 (inc ay) sz max)
              (let [a (get s [bx by] 0)
                    b (get s [(dec ax) by] 0)
                    c (get s [bx (dec ay)] 0)
                    d (get s [(dec ax) (dec ay)] 0)
                    t (+ (- a b c) d)]
                (if (> t (val max))
                  (recur (inc ax) ay sz (first {[ax ay (inc sz)] t}))
                  (recur (inc ax) ay sz max)))))))))

(let [in (first (line-seq (java.io.BufferedReader. *in*)))
      xs (sums (cell (parse-long in)))]
  (println "Part A:" (join "," (take 2 (search xs 3 3))))
  (println "Part B:" (join "," (take 3 (search xs 3 300)))))
