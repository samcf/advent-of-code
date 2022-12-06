(def xf-line-crate
  (comp (map (fn [ln] (str ln " ")))
        (map (fn [ln] (partition 4 ln)))
        (map (fn [ln] (map second ln)))))

(def xf-column
  (comp (map rseq)
        (map #(take-while (fn [x] (not= x \space)) %))
        (map vec)))

(def xf-line-move
  (comp (map #(re-seq #"\d+" %))
        (map #(map (fn [x] (Integer. x)) %))
        (map (fn [[n src dst]] [n (dec src) (dec dst)]))))

(defn move-crates-fn [stack-fn]
  (fn [r [n src dst]]
    (let [idx (- (count (r src)) n)]
      (-> (update r src subvec 0 idx)
          (update dst into (stack-fn (subvec (r src) idx)))))))

(let [lns (line-seq (java.io.BufferedReader. *in*))
      [crt [_ _ & mvs]] (split-at 8 lns)
      crt (->> (sequence xf-line-crate crt) (apply mapv vector) (into [] xf-column))
      mvs (sequence xf-line-move mvs)]
  (println "Part A:" (transduce (map last) str (reduce (move-crates-fn rseq) crt mvs)))
  (println "Part B:" (transduce (map last) str (reduce (move-crates-fn identity) crt mvs))))
