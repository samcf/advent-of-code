(defn tls? [xs]
  (loop [idx 0 ex? false in? false hype? false]
    (cond
      (>= (+ idx 3) (count xs)) (and ex? (not in?))
      (= (xs idx) \[) (recur (inc idx) ex? in? true)
      (= (xs idx) \]) (recur (inc idx) ex? in? false)
      :else
      (let [a (xs idx) b (xs (+ idx 1)) c (xs (+ idx 2)) d (xs (+ idx 3))]
        (if (and (= a d) (= b c) (not= a b))
          (if hype?
            (recur (+ idx 4) ex? true hype?)
            (recur (+ idx 4) true in? hype?))
          (recur (inc idx) ex? in? hype?))))))

(defn ssl? [xs]
  (loop [idx 0 ex (list) in (list) hype false]
    (cond
      (>= (+ idx 2) (count xs))
      (some
       (fn [[a b c]]
         (some
          (fn [[d e f]]
            (and (= a e c) (= b d f))) ex)) in)
      (= (xs idx) \[) (recur (inc idx) ex in true)
      (= (xs idx) \]) (recur (inc idx) ex in false)
      :else
      (let [a (xs idx) b (xs (+ idx 1)) c (xs (+ idx 2))]
        (if (and (= a c) (not= a b))
          (if hype
            (recur (inc idx) ex (conj in [a b c]) hype)
            (recur (inc idx) (conj ex [a b c]) in hype))
          (recur (inc idx) ex in hype))))))

(defn counting [pred]
  (keep (fn [x] (when (pred x) 1))))

(let [in (line-seq (java.io.BufferedReader. *in*))]
  (println "Part A:" (transduce (comp (map vec) (counting tls?)) + in))
  (println "Part B:" (transduce (comp (map vec) (counting ssl?)) + in)))
