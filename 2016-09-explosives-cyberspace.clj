(defn decompress [xs src dst len-fn]
  (loop [idx src enc nil sum 0]
    (cond
      (= idx dst) sum
      (= (xs idx) \() (recur (inc idx) "" sum)
      (= (xs idx) \))
      (let [[len mul] (map parse-long (re-seq #"\d+" enc))]
        (recur (+ idx (inc len)) nil (+ (* (len-fn xs idx len) mul) sum)))
      (string? enc) (recur (inc idx) (str enc (xs idx)) sum)
      :else
      (recur (inc idx) enc (inc sum)))))

(defn recursive [xs idx len]
  (decompress xs (inc idx) (+ (inc idx) len) recursive))

(let [in (line-seq (java.io.BufferedReader. *in*))
      xs (into [] (seq (first in)))]
  (println "Part A:" (decompress xs 0 (count xs) (fn [_ _ len] len)))
  (println "Part B:" (decompress xs 0 (count xs) recursive)))
