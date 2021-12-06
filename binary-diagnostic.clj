(ns advent)

(defn a [coll]
  (let [[digits _] (first coll)
        mask       (bit-shift-left 1 digits)
        gamma      (->> (reverse (range 1 (+ digits 1)))
                        (map (fn [n] (bit-shift-left 1 (- n 1))))
                        (reduce
                         (fn [g m]
                           (if (> (reduce (fn [c [_ n]] (if (= (bit-and m n) 0) c (inc c))) 0 coll) (/ (count coll) 2))
                             (bit-or g m) g)) 0))]
    (* gamma (bit-and (bit-not gamma) (- mask 1)))))

(defn b [coll] 0)

(def reports
  (into [] (map (juxt count #(Integer/parseInt % 2))) (line-seq (java.io.BufferedReader. *in*))))

(println "Part A:" (a reports))
(println "Part B:" (b reports))
