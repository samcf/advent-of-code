(defn parse [sz]
  (let [point (fn [idx] [(mod idx sz) (quot idx sz)])]
    (comp cat
          (map-indexed vector)
          (remove (comp #{\.} second))
          (map (juxt second (comp point first))))))

(defn locations
  ([xs] xs)
  ([xs [k v]] (update xs k (fnil conj []) v)))

(defn solve [xs steps size]
  (for [[ax ay] xs
        [bx by] xs
        step steps
        :let [cx (+ ax (* step (- ax bx)))
              cy (+ ay (* step (- ay by)))]
        :while (and (not= ax bx)
                    (not= ay by)
                    (>= cx 0)
                    (>= cy 0)
                    (< cx size)
                    (< cy size))]
    (+ cx (* cy size))))

(let [in (line-seq (java.io.BufferedReader. *in*))
      sz (count in)
      xs (transduce (parse sz) locations {} in)
      xf (fn [steps]
           (comp (remove (comp #{\.} key))
                 (filter (comp (fn [x] (> x 1)) count val))
                 (mapcat (fn [[_ xs]] (solve xs steps sz)))
                 (distinct)
                 (map (constantly 1))))]
  (println "Part A:" (transduce (xf (list 1))   + xs))
  (println "Part B:" (transduce (xf (range sz)) + xs)))
