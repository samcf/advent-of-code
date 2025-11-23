(defn reflect [idx n]
  (let [p (* 2 (dec n))
        i (mod idx p)]
    (if (< i n) i (- p i))))

(defn caught?
  ([x]   (zero? (reflect (key x) (val x))))
  ([t x] (zero? (reflect (+ (key x) t) (val x)))))

(defn severity [x]
  (if (caught? x)
    (* (key x) (val x)) 0))

(let [in (line-seq (java.io.BufferedReader. *in*))
      xf (comp (map (fn [ln] (re-seq #"\d+" ln)))
               (map (fn [xs] (into [] (map parse-long) xs))))
      xs (into {} xf in)]
  (println "Part A:" (transduce (map severity) + xs))
  (println "Part B:" (first (remove (fn [t] (some (fn [x] (caught? t x)) xs)) (range)))))
