(def dict {\F 0 \B 1 \L 0 \R 1})

(defn search [xs]
  (reduce (fn [r x] (+ (* 2 r) x)) 0 xs))

(defn seat [xs]
  (-> (* (search (take 7 xs)) 8)
      (+ (search (take-last 3 xs)))))

(defn gap [[a b & rest]]
  (if (= (- a 2) b) (- a 1) (gap (conj rest b))))

(let [in (line-seq (java.io.BufferedReader. *in*))
      xf (comp (map #(map dict %)) (map seat))
      xs (into (sorted-set-by >) xf in)]
  (println "Part A:" (first xs))
  (println "Part B:" (gap xs)))
