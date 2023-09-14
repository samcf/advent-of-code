(require '[clojure.string :refer [replace]]
         '[clojure.math.combinatorics :refer [cartesian-product]])

(defn partition-before [pred]
  (comp (partition-by pred) (partition-all 2) (map (partial apply into))))

(defn parse [s]
  (condp re-find s
    #"mem\[(\d+)\] = (\d+)" :>> (fn [[_ x y]] [(Integer. x) (Integer. y)])
    #"mask = (\w+)"         :>> (fn [[_ x]] x)))

(defn apply-mask [s x]
  (let [a (Long/parseLong (replace s #"X" "0") 2)
        b (Long/parseLong (replace s #"[X10]" {"X" "0" "1" "0" "0" "1"}) 2)]
    (- (bit-or x a) (bit-and x b))))

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
    (sequence xf (apply cartesian-product (repeat xs [\1 \0])))))

(defn solve-a
  ([r] (transduce (map val) + r))
  ([r [s & xs]]
   (let [f (fn [[_ x]] (apply-mask s x))]
     (into r (map (juxt first f)) xs))))

(defn solve-b
  ([r] (transduce (map val) + r))
  ([r [s & xs]]
   (let [a (Long/parseLong (replace s #"X" "0") 2)
         c (combinations (replace s #"0" "Y"))
         f (fn [[k x]]
             (let [xf (comp (map (fn [s] (replace s #"Y" "X")))
                            (map (fn [s] (bit-or (apply-mask s k) a)))
                            (map (juxt identity (constantly x))))]
               (sequence xf c)))]
     (into r (mapcat f) xs))))

(let [in (line-seq (java.io.BufferedReader. *in*))
      xf (comp (map parse) (partition-before string?))]
  (println "Part A:" (transduce xf solve-a {} in))
  (println "Part B:" (transduce xf solve-b {} in)))
