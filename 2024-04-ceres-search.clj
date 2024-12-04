(defn count-a [xs idx len]
  (let [offs [(- len) (inc (- len)) 1 (inc len) len (dec len) -1 (dec (- len))]
        iter (range 1 4)
        xfrm (comp (map (fn [offs] (map (fn [itr] (* offs itr)) iter)))
                   (map (fn [offs] (map (fn [off] (+ idx off)) offs)))
                   (map (fn [idxs] (map (fn [idx] (get xs idx)) idxs)))
                   (map (fn [vals] (apply str vals)))
                   (filter #{"MAS"})
                   (map (constantly 1)))]
    (transduce xfrm + offs)))

(defn count-b [xs idx len]
  (let [offs [(dec (- len)) (inc (- len)) (dec len) (inc len)]
        vals (map (fn [off] (get xs (+ idx off))) offs)]
    (if (#{"SSMM" "SMSM" "MMSS" "MSMS"} (apply str vals))
      1 0)))

(let [in (line-seq (java.io.BufferedReader. *in*))
      xs (into [] (mapcat (fn [s] (str s \.))) in)
      rs (range (count xs))
      xf (fn [c f]
           (comp (filter (fn [idx] (= (get xs idx) c)))
                 (map    (fn [idx] (f xs idx (inc (count (first in))))))))]
  (println "Part A:" (transduce (xf \X count-a) + rs))
  (println "Part B:" (transduce (xf \A count-b) + rs)))
