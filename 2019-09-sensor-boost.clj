(require '[clojure.core.async :as async]
         '[intcode :refer [intcode]])

(defn run [xs mode]
  (let [[_ dst out] (intcode xs mode)]
    (async/close! dst)
    (async/<!! out)))

(let [in (first (line-seq (java.io.BufferedReader. *in*)))
      xf (comp (map parse-long) (map-indexed vector))
      xs (into {} xf (re-seq #"-?\d+" in))]
  (println "Part A:" (run xs 1))
  (println "Part B:" (run xs 2)))
