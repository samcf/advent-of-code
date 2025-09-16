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

(defn play [xs]
  (let [[src dst out] (intcode xs)]
    (go-loop [rs {} ax 0 bx 0 sum 0]
      (let [sx (long (signum (- ax bx)))]
        (alt!
          [[src sx]] (recur rs ax bx sum)
          out sum
          dst
          ([x]
           (let [y (<! dst) t (<! dst)]
             (if (and (= x -1) (= y 0))
               (recur rs ax bx t)
               (let [rs (assoc rs [x y] t)]
                 (case t
                   3 (recur rs ax x sum)
                   4 (recur rs x bx sum)
                   (recur rs ax bx sum)))))))))))

(let [in (first (line-seq (java.io.BufferedReader. *in*)))
      re (re-seq #"-?\d+" in)
      xf (comp (map parse-long) (map-indexed vector))
      xs (into {} xf re)]
  (println "Part A:" (<!! (blocks xs)))
  (println "Part B:" (<!! (play (assoc xs 0 2)))))
