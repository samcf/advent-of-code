(defn group [m k s]
  (let [s (conj s k)
        f (comp
           (remove s)
           (mapcat (fn [k] (group m k s))))]
    (into s f (m k))))

(defn groups [m r]
  (if (seq m)
    (let [s (group m (key (first m)) (hash-set))]
      (recur (apply dissoc m s) (conj r s))) r))

(let [in (line-seq (java.io.BufferedReader. *in*))
      xf (comp (map (fn [ln] (re-seq #"\d+" ln)))
               (map (fn [xs] (map parse-long xs)))
               (map (juxt first rest)))
      xs (into {} xf in)]
  (println "Part A:" (count (group xs 0 (hash-set))))
  (println "Part B:" (count (groups xs (list)))))
