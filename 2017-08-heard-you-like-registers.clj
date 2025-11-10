(require '[clojure.string :refer [split]])

(def operator
  {"inc" + "dec" -})

(def comparison
  {"==" = "!=" not= "<" < ">" > "<=" <= ">=" >=})

(defn parse [ln]
  (let [[a b c _ d e f] (split ln #" ")]
    [a (operator b) (parse-long c) d (comparison e) (parse-long f)]))

(defn run
  ([] {:r {} :t ##-Inf})
  ([rs [a b c d e f]]
   (if (e (get (:r rs) d 0) f)
     (let [v (b (get (:r rs) a 0) c)]
       (update (update rs :r assoc a v) :t max v)) rs)))

(let [in (line-seq (java.io.BufferedReader. *in*))
      cf (fn [m] (reduce max (vals (:r m))))]
  (println "Part A:" (transduce (map parse) (completing run cf) in))
  (println "Part B:" (transduce (map parse) (completing run :t) in)))
