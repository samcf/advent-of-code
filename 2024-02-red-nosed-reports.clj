(defn exactly? [xs]
  (and (or (apply > xs) (apply < xs))
       (loop [xs xs]
         (if (seq (rest xs))
           (if (<= (abs (- (first xs) (second xs))) 3)
             (recur (rest xs))
             false)
           true))))

(defn tolerated? [xs]
  (loop [hd (list) tl xs]
    (if (exactly? (into tl (rest hd))) true
        (if (seq tl)
          (recur (conj hd (first tl)) (rest tl))
          false))))

(let [in (line-seq (java.io.BufferedReader. *in*))
      xf (fn [pred]
           (comp (map (fn [ln] (re-seq #"\d+" ln)))
                 (map (fn [ln] (map parse-long ln)))
                 (filter pred)
                 (map (constantly 1))))]
  (println "Part A:" (transduce (xf exactly?)   + in))
  (println "Part B:" (transduce (xf tolerated?) + in)))
