(require '[clojure.string :refer [replace]])

(defn solve [xs]
  (loop [xs xs rs 0 op +]
    (if (seq xs)
      (let [[x & xs] xs]
        (case x
          \( (let [[r xs] (solve xs)] (recur xs (op rs r) op))
          \) [rs xs]
          \+ (recur xs rs +)
          \* (recur xs rs *)
          (recur xs (op rs (Integer. (str x))) op))) rs)))

(let [in (line-seq (java.io.BufferedReader. *in*))
      xs (comp (map (fn [s] (replace s #" " "")))
               (map seq)
               (map solve))]
  (println "Part A:" (transduce xs + in)))
