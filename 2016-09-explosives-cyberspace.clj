(defn join [x c]
  (+ (* x 10) (- (int c) 48)))

(defn decompress [xs src dst sum-fn]
  (loop [idx src len nil mul nil sum 0]
    (if (= idx dst) sum
        (let [val (xs idx)]
          (cond
            (= val \() (recur (inc idx) 0 mul sum)
            (= val \)) (recur (+ idx (inc len)) nil nil (+ (* (sum-fn xs (inc idx) len) mul) sum))
            (= val \x) (recur (inc idx) len 0 sum)
            mul        (recur (inc idx) len (join mul val) sum)
            len        (recur (inc idx) (join len val) mul sum)
            :else      (recur (inc idx) len mul (inc sum)))))))

(defn recursive [xs idx len]
  (decompress xs idx (+ idx len) recursive))

(let [in (line-seq (java.io.BufferedReader. *in*))
      xs (into [] (first in))]
  (println "Part A:" (decompress xs 0 (count xs) (fn [_ _ len] len)))
  (println "Part B:" (decompress xs 0 (count xs) recursive)))
