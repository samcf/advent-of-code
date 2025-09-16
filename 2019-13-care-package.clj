(require '[clojure.core.async :refer [<!! <! go-loop alt!]]
         '[clojure.math :refer [signum]]
         '[intcode :refer [intcode]])

(defn blocks [xs]
  (let [[_ dst out] (intcode xs)]
    (go-loop [sum 0]
      (alt!
        out sum
        dst
        ([_]
         (let [_ (<! dst) t (<! dst)]
           (recur (if (= t 2) (inc sum) sum))))))))

(defn score [xs]
  (let [[src dst out] (intcode xs)]
    (go-loop [ax 0 bx 0 sum 0]
      (let [sx (long (signum (- ax bx)))]
        (alt!
          [[src sx]] (recur ax bx sum)
          out sum
          dst ([x]
               (let [_ (<! dst) t (<! dst)]
                 (cond
                   (= x -1) (recur ax bx t)
                   (= t  3) (recur ax x sum)
                   (= t  4) (recur x bx sum)
                   :else    (recur ax bx sum)))))))))

(let [in (first (line-seq (java.io.BufferedReader. *in*)))
      re (re-seq #"-?\d+" in)
      xf (comp (map parse-long) (map-indexed vector))
      xs (into {} xf re)]
  (println "Part A:" (<!! (blocks xs)))
  (println "Part B:" (<!! (score (assoc xs 0 2)))))
