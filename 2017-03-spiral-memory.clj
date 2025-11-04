(def nbhd [[0 -1] [1 -1] [1 0] [1 1] [0 1] [-1 1] [-1 0] [-1 -1]])
(def prev {[1 0] [0 1] [0 -1] [1 0] [-1 0] [0 -1] [0 1] [-1 0]})

(def spiral
  (->> (cycle [[1 0] [0 -1] [-1 0] [0 1]])
       (interleave (drop 2 (interleave (range) (range))))
       (partition 2)))

(defn man [[ax ay]]
  (+ (abs ax) (abs ay)))

(defn add [[ax ay] [bx by]]
  [(+ ax bx) (+ ay by)])

(defn mul [[ax ay] n]
  [(* ax n) (* ay n)])

(defn distance [t]
  (reduce
   (fn [[n pos] [s dir]]
     (if (> n t)
       (reduced (man (add (mul (prev dir) (* (- n t) -1)) pos)))
       [(+ n s) (add pos (mul dir s))])) [1 [0 0]] spiral))

(defn stress [t]
  (loop [kvs {[0 0] 1}
         pos [0 0]
         mov spiral
         dst (ffirst mov)]
    (if (zero? dst)
      (recur kvs pos (rest mov) (ffirst (rest mov)))
      (let [pos (add pos (second (first mov)))
            vxf (comp (map (partial add pos)) (map kvs) (filter number?))
            val (transduce vxf + nbhd)]
        (if (> val t) val
            (recur (assoc kvs pos val) pos mov (dec dst)))))))

(let [in (first (line-seq (java.io.BufferedReader. *in*)))
      xs (parse-long (re-find #"\d+" in))]
  (println "Part A:" (distance xs))
  (println "Part B:" (stress xs)))
