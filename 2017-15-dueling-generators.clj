(defn generate [a b c d]
  (loop [a a]
    (let [a (rem (* a b) c)]
      (if (zero? (mod a d)) a
          (recur a)))))

(defn matches [a b c d n]
  (loop [a a b b i 0 t 0]
    (if (> i n) t
        (recur
         (generate a 16807 2147483647 c)
         (generate b 48271 2147483647 d)
         (inc i)
         (if (= (bit-and a 0xFFFF) (bit-and b 0xFFFF))
           (inc t) t)))))

(let [in (line-seq (java.io.BufferedReader. *in*))
      re (re-seq #"\d+" (apply str in))
      [a b] (map parse-long re)]
  (println "Part A:" (matches a b 1 1 4e7))
  (println "Part B:" (matches a b 4 8 5e6)))
