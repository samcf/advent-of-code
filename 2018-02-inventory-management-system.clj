(def freqs
  (comp (map frequencies) (map vals) (map set)))

(defn checksum
  ([] {:a 0 :b 0})
  ([m] (* (:a m) (:b m)))
  ([m s]
   (cond-> m
     (some #{2} s) (update :a inc)
     (some #{3} s) (update :b inc))))

(defn diff [a b]
  (loop [idx 0 pos nil]
    (if (= idx (count a)) pos
        (if (= (nth a idx) (nth b idx))
          (recur (inc idx) pos)
          (if (some? pos) nil
              (recur (inc idx) idx))))))

(defn common [xs]
  (first
   (for [a xs b xs :let [pos (diff a b)] :when pos]
     (keep-indexed
      (fn [idx chr]
        (when (not= idx pos) chr)) a))))

(let [in (line-seq (java.io.BufferedReader. *in*))]
  (println "Part A:" (transduce freqs checksum in))
  (println "Part B:" (apply str (common in))))
