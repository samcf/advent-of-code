(require '[clojure.core.async :as async]
         '[intcode :refer [intcode]])

(defn run [xs id]
  (let [[src dst out] (intcode xs)]
    (async/close! dst)
    (async/>!! src id)
    (async/<!! out)))

(let [in (first (line-seq (java.io.BufferedReader. *in*)))
      xs (into [] (map parse-long) (re-seq #"-?\d+" in))]
  (println "Part A:" (run xs 1))
  (println "Part B:" (run xs 5)))
