(require '[clojure.string :refer [split]])

(def xf-line-crate
  (comp (map (fn [ln] (str ln " ")))
        (map (fn [ln] (partition 4 ln)))
        (map (fn [ln] (map second ln)))))

(def xf-column
  (comp (map rseq)
        (map #(take-while (fn [x] (not= x \space)) %))
        (map vec)))

(def xf-line-move
  (comp (map (fn [ln] (split ln #" ")))
        (map (fn [[_ n _ src _ dst]]
               [(Integer. n)
                (- (Integer. src) 1)
                (- (Integer. dst) 1)]))))

(defn move-crates-fn [stack-fn]
  (fn [r [n s d]]
    (-> (update r s subvec 0 (- (count (r s)) n))
        (update d (partial apply conj) (stack-fn (subvec (r s) (- (count (r s)) n)))))))

(let [lns (line-seq (java.io.BufferedReader. *in*))
      [crt [_ _ & mvs]] (split-at 8 lns)
      crt (->> (sequence xf-line-crate crt) (apply mapv vector) (into [] xf-column))
      mvs (sequence xf-line-move mvs)
      res (fn [stack-fn] (reduce (move-crates-fn stack-fn) crt mvs))]
  (println "Part A:" (transduce (map last) str (res rseq)))
  (println "Part B:" (transduce (map last) str (res identity))))
