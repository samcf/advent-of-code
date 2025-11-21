(def start {:q 0 :r 0 :s 0})

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

(defn farthest
  ([] {:p start :d 0})
  ([rs] (:d rs))
  ([rs c]
   (let [p (add (:p rs) c)]
     (update (assoc rs :p p) :d max (distance p)))))

(let [in (first (line-seq (java.io.BufferedReader. *in*)))
      xs (re-seq #"\w+" in)]
  (println "Part A:" (transduce (map dirs) (completing add distance) start xs))
  (println "Part B:" (transduce (map dirs) farthest xs)))
