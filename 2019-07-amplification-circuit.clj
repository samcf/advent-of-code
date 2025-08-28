(require '[clojure.core.async :as async]
         '[intcode :refer [intcode]]
         '[permutations :refer [permutations]])

(defn run [xs [a b c d e]]
  (let [[a-src a-dst] (intcode xs)
        [b-src b-dst] (intcode xs)
        [c-src c-dst] (intcode xs)
        [d-src d-dst] (intcode xs)
        [e-src e-dst out] (intcode xs)]
    (async/pipe a-dst b-src)
    (async/pipe b-dst c-src)
    (async/pipe c-dst d-src)
    (async/pipe d-dst e-src)
    (async/pipe e-dst a-src)
    (async/>!! e-src e)
    (async/>!! d-src d)
    (async/>!! c-src c)
    (async/>!! b-src b)
    (async/>!! a-src a)
    (async/>!! a-src 0)
    (async/<!! out)))

(let [in (first (line-seq (java.io.BufferedReader. *in*)))
      xs (into [] (map parse-long) (re-seq #"-?\d+" in))
      xf (map (fn [ys] (run xs ys)))]
  (println "Part A:" (transduce xf max 0 (permutations (list 0 1 2 3 4))))
  (println "Part B:" (transduce xf max 0 (permutations (list 5 6 7 8 9)))))
