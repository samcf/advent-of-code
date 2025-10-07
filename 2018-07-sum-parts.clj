(defn available [xs]
  (into
   (sorted-set)
   (comp (filter (comp not seq val)) (map key)) xs))

(defn prune [xs & steps]
  (update-vals xs (fn [deps] (apply disj deps steps))))

(defn path [xs]
  (loop [xs xs steps []]
    (if (not (seq xs)) steps
        (let [step (first (available xs))]
          (recur (prune (dissoc xs step) step) (conj steps step))))))

(defn duration [xs n t]
  (loop [xs xs wk {} time 0]
    (if (or (seq xs) (seq wk))
      (let [dn (reduce
                (fn [r [k v]]
                  (if (= (+ -64 k t v) time)
                    (conj r k) r)) (list) wk)
            xs (apply prune xs dn)
            wk (apply dissoc wk dn)
            av (take (- n (count wk)) (available xs))
            xs (apply dissoc xs av)
            wk (into wk (map (fn [step] [step time])) av)]
        (recur xs wk (inc time)))
      (dec time))))

(let [in (line-seq (java.io.BufferedReader. *in*))
      xf (comp (map (fn [ln] (rest (re-seq #"[A-Z]" ln))))
               (map (fn [xs] (map (comp int first) xs)))
               (map (fn [[a b]] {a #{} b #{a}})))
      xs (apply merge-with into (sequence xf in))]
  (println "Part A:" (transduce (map char) str (path xs)))
  (println "Part B:" (duration xs 5 60)))
