(require '[clojure.string :refer [join]])

(defn cell [n]
  (fn [x y]
    (let [a (+ x 10)]
      (-> (* a y) (+ n) (* a) (quot 100) (mod 10) (- 5)))))

(defn sums [f]
  (loop [y 1 x 1 s {}]
    (if (> y 300) s
        (if (> x 300)
          (recur (inc y) 1 s)
          (let [p (f x y)
                a (get s [(dec x) y] 0)
                b (get s [x (dec y)] 0)
                c (get s [(dec x) (dec y)] 0)
                t (- (+ a b p) c)]
            (recur y (inc x) (assoc s [x y] t)))))))

(defn search [s j k]
  (loop [ax 1 ay 1 sz (dec j) max (first {[0 0 0] 0})]
    (let [bx (+ ay sz) by (+ ax sz)]
      (if (= sz k) (key max)
          (if (> by 300)
            (recur 1 1 (inc sz) max)
            (if (> bx 300)
              (recur (inc ax) 1 sz max)
              (let [a (get s [bx by] 0)
                    b (get s [(dec ay) by] 0)
                    c (get s [bx (dec ax)] 0)
                    d (get s [(dec ay) (dec ax)] 0)
                    t (+ (- a b c) d)]
                (if (> t (val max))
                  (recur ax (inc ay) sz (first {[ay ax (inc sz)] t}))
                  (recur ax (inc ay) sz max)))))))))

(let [in (first (line-seq (java.io.BufferedReader. *in*)))
      xs (sums (cell (parse-long in)))]
  (println "Part A:" (join "," (take 2 (search xs 3 3))))
  (println "Part B:" (join "," (take 3 (search xs 3 300)))))
