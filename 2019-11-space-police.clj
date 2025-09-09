(require '[clojure.core.async :as async]
         '[intcode :refer [intcode]])

(def turn
  [{[0 -1] [-1 0] [-1 0] [0 1] [0 1] [1  0] [1  0] [0 -1]}
   {[0 -1] [1  0] [1  0] [0 1] [0 1] [-1 0] [-1 0] [0 -1]}])

(defn move [[ax ay] [bx by]]
  [(+ ax bx) (+ ay by)])

(defn color [x]
  (if (= x 0) \░ \█))

(defn length [f xs]
  (range (inc (transduce (map (comp f key)) max 0 xs))))

(defn run [xs init]
  (let [[src dst out] (intcode xs)]
    (async/<!!
     (async/go-loop [pos [0 0] dir [0 -1] rs {[0 0] init}]
       (async/alt!
         [[src (get rs pos 0)]]
         (let [col (async/<! dst)
               rot (async/<! dst)
               dir ((turn rot) dir)]
           (recur (move pos dir) dir (assoc rs pos col)))
         out rs)))))

(let [in (first (line-seq (java.io.BufferedReader. *in*)))
      xf (comp (map parse-long) (map-indexed vector))
      xs (into {} xf (re-seq #"-?\d+" in))]
  (println "Part A:" (count (run xs 0)))
  (println "Part B:")
  (let [rs (run xs 1)]
    (doseq [y (length (fn [[_ y]] y) rs)]
      (println
       (transduce
        (comp (map (fn [x] (rs [x y]))) (map color)) str
        (length (fn [[x _]] x) rs))))))
