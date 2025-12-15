(import '[java.security MessageDigest])

(def inst (MessageDigest/getInstance "MD5"))

(defn encode [s n]
  (let [byt (.getBytes (str s n) "UTF-8")
        enc (.digest inst byt)
        hex (format "%32x" (java.math.BigInteger. 1 enc))]
    (.replace hex " " "0")))

(defn transform-a [id]
  (comp
   (map    (fn [n] (encode id n)))
   (filter (fn [s] (= (subs s 0 5) "00000")))
   (map    (fn [s] (subs s 5 6)))))

(defn transform-b [id]
  (comp
   (map    (fn [n] (encode id n)))
   (filter (fn [s] (= (subs s 0 5) "00000")))
   (map    (fn [s] [(parse-long (subs s 5 6)) (subs s 6 7)]))
   (filter (fn [[k _]] (some? k)))
   (filter (fn [[k _]] (<= 0 k 7)))))

(defn join-next
  ([] "")
  ([r] r)
  ([r s]
   (let [r (str r s)]
     (if (= (count r) 8)
       (reduced r) r))))

(defn join-kv
  ([] {})
  ([m] (apply str (vals (sort-by key m))))
  ([m [k v]]
   (if (m k) m
       (let [m (assoc m k v)]
         (if (= (count m) 8) (reduced m) m)))))

(let [in (first (line-seq (java.io.BufferedReader. *in*)))]
  (println "Part A:" (transduce (transform-a in) join-next (range)))
  (println "Part B:" (transduce (transform-b in) join-kv   (range))))
