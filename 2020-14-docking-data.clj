(defn parse [s]
  (condp re-find s
    #"mem\[(\d+)\] = (\d+)" :>> (fn [[_ x y]] [(Integer. x) (Integer. y)])
    #"mask = (\w+)"         :>> (comp seq second)))

(defn mask-a [xs]
  (fn [v]
    (loop [xs xs v v]
      (if (seq xs)
        (let [[x & xs] xs i (count xs)]
          (case x
            \0 (recur xs (bit-clear v i))
            \1 (recur xs (bit-set   v i))
            \X (recur xs v))) v))))

(defn solve-a
  ([result]
   (transduce (map val) + result))
  ([result [mask & assigns]]
   (let [xf (map (juxt first (comp (mask-a mask) second)))]
     (into result xf assigns))))

(defn mask-b [xs v]
  (if (seq xs)
    (let [[x & xs] xs i (count xs)]
      (case x
        \0 (mask-b xs v)
        \1 (mask-b xs (bit-set v i))
        \X (into (mask-b xs (bit-clear v i))
                 (mask-b xs (bit-set   v i))))) [v]))

(defn solve-b
  ([result]
   (transduce (map val) + result))
  ([result [mask & assigns]]
   (into result
         (comp (map (fn [[k v]] [(mask-b mask k) v]))
               (map (fn [[k v]] (map (fn [k] [k v]) k)))
               cat) assigns)))

(let [in (line-seq (java.io.BufferedReader. *in*))
      xf (comp (map parse)
               (partition-by seq?)
               (partition-all 2)
               (map (partial apply into)))]
  (println "Part A:" (transduce xf solve-a {} in))
  (println "Part B:" (transduce xf solve-b {} in)))
