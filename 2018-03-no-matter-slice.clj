(defn points [[_ x y w h]]
  (for [c (range w) r (range h)]
    [(+ x c) (+ r y)]))

(defn overlap
  ([]  {:a (hash-set) :b (hash-set)})
  ([m] (:b m))
  ([m v]
   (if ((:a m) v)
     (update m :b conj v)
     (update m :a conj v))))

(let [in (line-seq (java.io.BufferedReader. *in*))
      xf (comp (map (fn [ln] (re-seq #"\d+" ln)))
               (map (fn [xs] (map parse-long xs)))
               (map (juxt first points)))
      xs (into {} xf in)
      vs (transduce (mapcat val) overlap xs)]
  (println "Part A:" (count vs))
  (println "Part B:" (key (first (remove (fn [[_ v]] (some vs v)) xs)))))
