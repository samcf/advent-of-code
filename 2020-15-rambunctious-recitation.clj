(require '[clojure.string :refer [split]])

(defn conj' [xs x]
  (if (seq xs)
    (list x (first xs))
    (list x)))

(defn solve [xs n]
  (let [xf (comp (map-indexed vector) (map (juxt second (comp list inc first))))]
    (loop [dict (transient (into {} xf xs))
           turn (inc (count xs))
           prev (last xs)]
      (if (<= turn n)
        (let [hist (dict prev)
              next (if (= (count hist) 2) (apply - hist) 0)]
          (recur (assoc! dict next (conj' (dict next) turn))
                 (inc turn)
                 next))
        prev))))

(let [in (line-seq (java.io.BufferedReader. *in*))
      xf (comp (map (fn [s] (split s #","))) cat
               (map (fn [s] (Integer. s))))
      xs (sequence xf in)]
  (println "Part A:" (solve xs 2020))
  (println "Part B:" (solve xs 30000000)))
