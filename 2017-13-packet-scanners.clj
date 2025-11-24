(defn caught? [x t]
  (zero? (mod (+ (key x) t) (* (dec (val x)) 2))))

(defn severity [x]
  (if (caught? x 0)
    (* (key x) (val x)) 0))

(let [in (line-seq (java.io.BufferedReader. *in*))
      xf (comp (map (fn [ln] (re-seq #"\d+" ln)))
               (map (fn [xs] (into [] (map parse-long) xs))))
      xs (into {} xf in)]
  (println "Part A:" (transduce (map severity) + xs))
  (println "Part B:" (first
                      (remove
                       (fn [t] (some (fn [x] (caught? x t)) xs))
                       (range)))))
