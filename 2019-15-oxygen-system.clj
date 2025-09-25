(require '[clojure.core.async :refer [<! >! <!! go-loop]]
         '[intcode :refer [intcode]])

(def queue clojure.lang.PersistentQueue/EMPTY)
(def moves (list 1 2 3 4))
(def unit {1 [0 -1] 2 [0 1] 3 [-1 0] 4 [1 0]})
(def inst {[0 -1] 1 [0 1] 2 [-1 0] 3 [1 0] 4})
(def back {1 2 2 1 3 4 4 3})

(defn add [[ax ay] [bx by]]
  [(+ ax bx) (+ ay by)])

(defn sub [[[ax ay] [bx by]]]
  [(- bx ax) (- by ay)])

(defn shortcut [a b]
  (loop [i (dec (count a)) j (dec (count b))]
    (if (= (a i) (b j))
      (into (vec (rseq (subvec a (inc i)))) (subvec b j))
      (if (= j 0)
        (recur (dec i) (dec (count b)))
        (recur i (dec j))))))

(defn steps [path]
  (sequence
   (comp (map sub) (map inst))
   (partition 2 1 path)))

(defn run [src dst start]
  (go-loop [queue queue cmds moves path [start] visited (hash-set) time 0]
    (if (not (and (empty? queue) (empty? cmds)))
      (if-let [[cmd & cmds] (seq cmds)]
        (let [point (add (peek path) (unit cmd))]
          (if (visited point)
            (recur queue cmds path visited time)
            (do (>! src cmd)
                (case (<! dst)
                  0 (recur queue cmds path visited time)
                  1 (let [next (conj path point)]
                      (>! src (back cmd))
                      (<! dst)
                      (recur (conj queue next) cmds path visited (max (count next) time)))
                  2 {:point point :length (count path)}))))
        (let [next (peek queue)]
          (doseq [cmd (steps (shortcut path next))]
            (>! src cmd)
            (<! dst))
          (recur (pop queue) moves next (conj visited (peek path)) time)))
      (dec time))))

(let [in (first (line-seq (java.io.BufferedReader. *in*)))
      xs (into {} (comp (map parse-long) (map-indexed vector)) (re-seq #"-?\d+" in))
      [src dst _] (intcode xs)
      rs (<!! (run src dst [0 0]))]
  (println "Part A:" (:length rs))
  (println "Part B:" (<!! (run src dst (:point rs)))))
