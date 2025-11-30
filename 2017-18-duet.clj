(defn parse [s]
  (if-let [x (parse-long s)]
    x (symbol s)))

(defn value [m v]
  (if (symbol? v) (get (:reg m) v 0) v))

(def instructions
  {'snd (fn [m x _] (update (assoc  m :frq (value m x)) :pos inc))
   'set (fn [m x y] (update (update m :reg assoc  x (value m y)) :pos inc))
   'add (fn [m x y] (update (update m :reg update x (fnil + 0) (value m y)) :pos inc))
   'mul (fn [m x y] (update (update m :reg update x (fnil * 0) (value m y)) :pos inc))
   'mod (fn [m x y] (update (update m :reg update x (fnil rem 0) (value m y)) :pos inc))
   'rcv (fn [m _ _] (update m :pos inc))
   'jgz (fn [m x y] (update m :pos + (if (> (value m x) 0) (value m y) 1)))})

(defn receive [xs]
  (loop [m {:pos 0 :reg {} :frq nil}]
    (let [[ins x y] (xs (:pos m))]
      (if (= ins 'rcv)
        (:frq m)
        (recur ((instructions ins) m x y))))))

(let [in (line-seq (java.io.BufferedReader. *in*))
      xf (comp
          (map (fn [ln] (re-seq #"-?\w+" ln)))
          (map (fn [xs] (into [(symbol (first xs))] (map parse) (rest xs)))))
      xs (into [] xf in)]
  (println "Part A:" (receive xs))
  (println "Part B:" nil))
