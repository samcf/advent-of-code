(defn parse [ln]
  (let [[k v & xs] (re-seq #"\w+" ln)]
    {k {:weight (parse-long v) :children xs}}))

(defn root [rs]
  (first
   (remove
    (into (hash-set) (mapcat :children) (vals rs))
    (keys rs))))

(defn weight [rs]
  (fn [k]
    (let [v (rs k)]
      (transduce
       (map (weight rs)) + (:weight v)
       (:children v)))))

(defn variant [rs]
  (let [fq (frequencies (vals rs))]
    (when-let [a (first (filter (comp #{1} val) fq))]
      (let [b (first (filter (comp #{(key a)} val) rs))]
        (first
         {(key b)
          (- (val (first (dissoc rs (key b))))
             (key a))})))))

(defn balance [rs k v]
  (let [xs (into {} (map (juxt identity (weight rs))) (:children (rs k)))]
    (if-let [kv (variant xs)]
      (recur rs (key kv) (val kv))
      (+ (:weight (rs k)) v))))

(let [in (line-seq (java.io.BufferedReader. *in*))
      rs (into {} (map parse) in)]
  (println "Part A:" (root rs))
  (println "Part B:" (balance rs (root rs) 0)))
