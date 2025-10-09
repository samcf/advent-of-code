(defn sum [xs]
  (loop [idx 0 stack (list 0) sum 0]
    (if (not (seq stack)) sum
        (let [n (first stack)]
          (if (> n 0)
            (recur (+ idx n) (rest stack) (reduce + sum (subvec xs idx (+ idx n))))
            (let [c (xs idx) m (xs (inc idx))]
              (recur (+ idx 2) (into (conj (rest stack) m) (repeat c 0)) sum)))))))

(defn tree [xs idx]
  (let [a (xs idx) b (xs (inc idx))]
    (if (pos? a)
      (let [dsc (loop [idx (+ idx 2) dsc a rs []]
                  (if (zero? dsc) rs
                      (let [file (tree xs idx)]
                        (recur (+ idx (:len file)) (dec dsc) (conj rs file)))))
            len (transduce (map :len) + dsc)
            src (+ idx len 2)]
        {:len (+ 2 len b)
         :meta (subvec xs src (+ src b))
         :desc dsc})
      {:len (+ 2 b)
       :meta (subvec xs (+ idx 2) (+ idx 2 b))})))

(defn value [file]
  (if (not (seq (:desc file)))
    (reduce + (:meta file))
    (transduce
     (comp
      (map dec)
      (map (fn [idx] (get (:desc file) idx)))
      (filter identity)
      (map value)) + (:meta file))))

(let [in (first (line-seq (java.io.BufferedReader. *in*)))
      xs (into [] (map parse-long) (re-seq #"\d+" in))]
  (println "Part A:" (sum xs))
  (println "Part B:" (value (tree xs 0))))
