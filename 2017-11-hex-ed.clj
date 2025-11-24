(def dirs
  {"n"  {:q 0 :r -1 :s 1}
   "ne" {:q 1 :r -1 :s 0}
   "se" {:q 1 :r 0 :s -1}
   "s"  {:q 0 :r 1 :s -1}
   "sw" {:q -1 :r 1 :s 0}
   "nw" {:q -1 :r 0 :s 1}})

(defn add [a b]
  {:q (+ (:q a) (:q b))
   :r (+ (:r a) (:r b))
   :s (+ (:s a) (:s b))})

(defn distance [a]
  (quot (+ (abs (:q a)) (abs (:r a)) (abs (:s a))) 2))

(let [in (first (line-seq (java.io.BufferedReader. *in*)))
      xs (into [] (map dirs) (re-seq #"\w+" in))
      rs (into [] (map distance) (reductions add {:q 0 :r 0 :s 0} xs))]
  (println "Part A:" (peek rs))
  (println "Part B:" (reduce max rs)))
