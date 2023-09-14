(require '[clojure.string :refer [replace]]
         '[clojure.math.combinatorics :refer [cartesian-product]])

(defn parse [s]
  (condp re-find s
    #"mem\[(\d+)\] = (\d+)" :>> (fn [[_ x y]] [(Integer. x) (Integer. y)])
    #"mask = (\w+)"         :>> (fn [[_ x]] x)))

(defn zip-replace [pred xs ys]
  (loop [xs xs ys ys rs []]
    (if (seq xs)
      (if (seq ys)
        (if (pred (first xs))
          (recur (rest xs) (rest ys) (conj rs (first ys)))
          (recur (rest xs) ys (conj rs (first xs))))
        (into rs xs))
      rs)))

(defn combinations [mask]
  (let [xs (get (frequencies mask) \X)
        xf (comp (map (partial zip-replace #{\X} mask))
                 (map (partial apply str)))]
    (->> (repeat xs [\1 \0])
         (apply cartesian-product)
         (sequence xf))))

(defn solve-a
  ([r] (transduce (map val) + r))
  ([r [s & xs]]
   (let [o (Long/parseLong (replace s #"X" "0") 2)
         z (Long/parseLong (replace s #"[X10]" {"X" "0" "1" "0" "0" "1"}) 2)
         f (fn [[_ v]] (- (bit-or v o) (bit-and v z)))]
     (into r (map (juxt first f)) xs))))

(defn solve-b
  ([r] (transduce (map val) + r))
  ([r [s & xs]]
   (let [a (Long/parseLong (replace s #"X" "0") 2)
         c (combinations (replace s #"0" "Y"))
         p (fn [[x t]]
             (->> (map (fn [s] (replace s #"Y" "X")) c)
                  (map (fn [s]
                         (let [o (Long/parseLong (replace s #"X" "0") 2)
                               z (Long/parseLong (replace s #"[X10]" {"X" "0" "1" "0" "0" "1"}) 2)]
                           [(bit-or (- (bit-or x o) (bit-and x z)) a) t])))))]
     (into r (mapcat p) xs))))

(let [in (line-seq (java.io.BufferedReader. *in*))
      xf (comp (map parse)
               (partition-by string?)
               (partition-all 2)
               (map (partial apply into)))]
  (println "Part A:" (transduce xf solve-a {} in))
  (println "Part B:" (transduce xf solve-b {} in)))
