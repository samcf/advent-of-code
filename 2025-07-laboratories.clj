(defn parse [in]
  (reduce-kv
   (fn [m k v]
     (case v
       \S (assoc  m :start k)
       \^ (update m :splitters conj k) m))
   {:start 0 :splitters (sorted-set-by >)} in))

(defn classical [idxs len idx]
  (let [last (first idxs)]
    (loop [next (conj clojure.lang.PersistentQueue/EMPTY idx)
           vstd (hash-set)]
      (let [idx (peek next)]
        (cond
          (> idx last) (count vstd)
          (vstd idx)   (recur (pop next) vstd)
          (idxs idx)   (recur (conj (pop next) (+ idx -1 len) (+ idx 1 len)) (conj vstd idx))
          :else        (recur (conj (pop next) (+ idx len)) vstd))))))

(def quantum
  (memoize
   (fn [idxs len idx]
     (cond
       (> idx (first idxs)) 1
       (idxs idx)
       (+ (quantum idxs len (+ idx -1 len))
          (quantum idxs len (+ idx  1 len)))
       :else (recur idxs len (+ idx len))))))

(let [in (line-seq (java.io.BufferedReader. *in*))
      ln (count (first in))
      rs (parse (into [] (comp (take-nth 2) cat) in))]
  (println "Part A:" (classical (:splitters rs) ln (:start rs)))
  (println "Part B:" (quantum   (:splitters rs) ln (:start rs))))
