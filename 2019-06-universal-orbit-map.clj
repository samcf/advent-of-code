(defn solve [xs node dist]
  (reduce
   (fn [data node]
     (let [result (solve xs node (inc dist))
           merged (merge data result {:sum (+ (:sum data) (:sum result))})
           {:keys [jmp src dst] :as data} merged]
       (if (and (not jmp) src dst)
         (assoc data :jmp (+ (- src dist) (- dst dist) -2))
         data)))
   (cond-> {:dist dist :sum dist}
     (= (key node) "YOU") (assoc :src dist)
     (= (key node) "SAN") (assoc :dst dist))
   (select-keys xs (val node))))

(let [in (line-seq (java.io.BufferedReader. *in*))
      xf (comp (map (fn [s] (re-seq #"\w+" s)))
               (map (fn [[k v]] {k [v] v []})))
      xs (apply merge-with into (sequence xf in))
      rs (solve xs (find xs "COM") 0)]
  (println "Part A:" (:sum rs))
  (println "Part B:" (:jmp rs)))
