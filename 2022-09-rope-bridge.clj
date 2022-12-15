(require '[clojure.math :refer [signum]])

(defn parse-ln [ln]
  (condp re-find ln
    #"U (\d+)" :>> (fn [[_ n]] (repeat (Integer. n) [0 -1]))
    #"R (\d+)" :>> (fn [[_ n]] (repeat (Integer. n) [1  0]))
    #"D (\d+)" :>> (fn [[_ n]] (repeat (Integer. n) [0  1]))
    #"L (\d+)" :>> (fn [[_ n]] (repeat (Integer. n) [-1 0]))))

(defn move [[ax ay] [bx by]]
  (if (> (max (abs (- ax bx)) (abs (- ay by))) 1)
    [(+ (signum (- bx ax)) ax)
     (+ (signum (- by ay)) ay)]
    [ax ay]))

(defn follow [[[a :as h] [b :as t] & ts]]
  (if (seq t)
    (let [c (move b a)]
      (if (= c b)
        (conj ts t h)
        (conj (follow (conj ts (conj t c))) h)))
    (conj ts h)))

(let [xs (->> (line-seq (java.io.BufferedReader. *in*))
              (sequence (comp (map parse-ln) cat))
              (reduce (fn [[[[ax ay] :as h] & t] [ox oy]]
                        (let [u [(+ ax ox) (+ ay oy)]]
                          (follow (conj t (conj h u)))))
                      (repeat 10 (list [0 0])))
              (into [] (map #(count (distinct %)))))]
  (println "Part A:" (xs 1))
  (println "Part B:" (xs 9)))
