(require '[clojure.string :refer [replace]])

(defn token [c]
  (if (#{\( \) \* \+} c) c (Integer. (str c))))

(def parse
  (comp (map (fn [xs] (replace xs #" " "")))
        (map (fn [xs] (map token xs)))))

(defn evaluate [xs]
  (loop [xs xs rs 0 op +]
    (if (seq xs)
      (let [[x & xs] xs]
        (case x
          \( (let [[rx xs] (evaluate xs)] (recur xs (op rs rx) op))
          \) [rs xs]
          \+ (recur xs rs +)
          \* (recur xs rs *)
          (recur xs (op rs x) op))) rs)))

(defn rewrite [xs rs]
  (if (seq xs)
    (let [[x & xs] xs]
      (case x
        \( (let [[xs ys] (rewrite xs [])] (recur xs (conj rs (evaluate ys))))
        \) [xs rs]
        \+ (if (= (first xs) \()
             (let [[xs ys] (rewrite (rest xs) [])]
               (recur xs (conj (pop rs) (+ (peek rs) (evaluate ys)))))
             (recur (rest xs) (conj (pop rs) (+ (peek rs) (first xs)))))
        (recur xs (conj rs x)))) rs))

(let [in (line-seq (java.io.BufferedReader. *in*))
      xf (fn [f] (comp parse (map f) (map evaluate)))]
  (println "Part A:" (transduce (xf identity) + in))
  (println "Part B:" (transduce (xf (fn [xs] (rewrite xs []))) + in)))
