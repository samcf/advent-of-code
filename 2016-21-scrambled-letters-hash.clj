(defn parse [ln]
  (condp re-find ln
    #"swap position (\d) with position (\d)"   :>> (fn [[_ a b]] ['swpo (parse-long a) (parse-long b)])
    #"swap letter (\w) with letter (\w)"       :>> (fn [[_ a b]] ['swpl (first a) (first b)])
    #"rotate left (\d) step"                   :>> (fn [[_   a]] ['rotl (parse-long a)])
    #"rotate right (\d) step"                  :>> (fn [[_   a]] ['rotr (parse-long a)])
    #"rotate based on position of letter (\w)" :>> (fn [[_   a]] ['rotp (first a)])
    #"reverse positions (\d) through (\d)"     :>> (fn [[_ a b]] ['revr (parse-long a) (parse-long b)])
    #"move position (\d) to position (\d)"     :>> (fn [[_ a b]] ['move (parse-long a) (parse-long b)])))

(defn index [rs x]
  (reduce-kv
   (fn [_ idx val]
     (when (= val x)
       (reduced idx))) nil rs))

(defn swap [rs a b]
  (assoc rs a (rs b) b (rs a)))

(defn rotate [rs n]
  (reduce-kv
   (fn [xs idx _]
     (assoc xs idx (rs (mod (- idx n) (count rs))))) rs rs))

(defn revert [rs a b]
  (reduce-kv
   (fn [xs idx _]
     (if (and (>= idx a) (<= idx b))
       (assoc xs idx (rs (- (+ a b) idx)))
       xs)) rs rs))

(defn move [rs a b]
  (reduce-kv
   (fn [xs idx _]
     (cond
       (and (< a b) (>= idx a) (< idx b)) (assoc xs idx (rs (inc idx)))
       (and (> a b) (> idx b) (<= idx a)) (assoc xs idx (rs (dec idx)))
       (= idx b)                          (assoc xs idx (rs a))
       :else xs)) rs rs))

(defn scramble
  ([xs] (apply str xs))
  ([xs [ins a b]]
   (case ins
     swpo (swap xs a b)
     swpl (swap xs (index xs a) (index xs b))
     revr (revert xs a b)
     move (move xs a b)
     rotl (rotate xs (- a))
     rotr (rotate xs a)
     rotp (let [n (index xs a)]
            (if (>= n 4)
              (rotate xs (+ n 2))
              (rotate xs (+ n 1)))))))

(defn unscramble
  ([xs] (apply str xs))
  ([xs [ins a b]]
   (case ins
     swpo (swap xs a b)
     swpl (swap xs (index xs a) (index xs b))
     revr (revert xs a b)
     move (move xs b a)
     rotl (rotate xs a)
     rotr (rotate xs (- a))
     rotp (let [n (index xs a)]
            (if (even? n)
              (rotate xs (- ({0 1 2 6 4 7 6 0} n)))
              (rotate xs (- (quot (inc n) 2))))))))

(let [in (line-seq (java.io.BufferedReader. *in*))]
  (println "Part A:" (transduce (map parse) scramble   [\a \b \c \d \e \f \g \h] in))
  (println "Part B:" (transduce (map parse) unscramble [\f \b \g \d \c \e \a \h] (reverse in))))
