(import '[java.security MessageDigest])

(def encode
  (let [inst (MessageDigest/getInstance "MD5")]
    (memoize
     (fn [s n]
       (loop [s s n n]
         (if (zero? n) s
             (recur (->> (.getBytes s "UTF-8") (.digest inst) (BigInteger. 1) (format "%032x"))
                    (dec n))))))))

(def run-three
  (memoize
   (fn [s]
     (loop [s s a nil b nil]
       (if-let [c (first s)]
         (if (= a b c) c
             (recur (rest s) b c)) nil)))))

(def run-five
  (memoize
   (fn [s x]
     (loop [s s a nil b nil c nil d nil]
       (if-let [e (first s)]
         (if (= x a b c d e) x
             (recur (rest s) b c d e)) nil)))))

(defn key? [salt idx rep]
  (if-let [char (run-three (encode (str salt idx) rep))]
    (loop [off 1]
      (if (> off 1000) false
          (if (run-five (encode (str salt (+ idx off)) rep) char) true
              (recur (inc off))))) false))

(defn solve [salt rep]
  (loop [idx 0 keys 0]
    (if (= keys 64) (dec idx)
        (if (key? salt idx rep)
          (recur (inc idx) (inc keys))
          (recur (inc idx) keys)))))

(let [in (first (line-seq (java.io.BufferedReader. *in*)))]
  (println "Part A:" (solve in 1))
  (println "Part B:" (solve in 2017)))
