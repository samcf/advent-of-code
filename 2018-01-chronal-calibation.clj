(defn freq
  ([] {:r 0 :d (hash-set)})
  ([r] r)
  ([m x]
   (let [r (+ (:r m) x)]
     (if ((:d m) r)
       (reduced r)
       (update (assoc m :r r) :d conj r)))))

(let [in (line-seq (java.io.BufferedReader. *in*))
      xf (comp (map (fn [s] (re-seq #"-?\d+" s)))
               (map first)
               (map parse-long))]
  (println "Part A:" (transduce xf + in))
  (println "Part B:" (transduce xf freq (cycle in))))
