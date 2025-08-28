(ns intcode (:require [clojure.core.async :as async]))

(defn- fill [xs n]
  (concat (repeat (- n (count xs)) 0) xs))

(defn- split [x]
  (loop [x x rs (list)]
    (if (> x 0) (recur (quot x 10) (conj rs (mod x 10)))
        rs)))

(defn ^:export intcode [xs & init]
  (let [xs (transient xs)
        <- (fn [idx mode] (if (= mode 0) (xs (xs idx)) (xs idx)))
        src (async/chan)
        dst (async/chan)]
    (async/onto-chan!! src init false)
    (->> (async/go-loop [rs nil ix 0 xs xs]
           (let [[_ f e _ instr] (fill (split (xs ix)) 5)
                 a (+ ix 1)
                 b (+ ix 2)
                 c (+ ix 3)
                 d (+ ix 4)]
             (case instr
               1 (recur rs d (assoc! xs (xs c) (+ (<- a e) (<- b f))))
               2 (recur rs d (assoc! xs (xs c) (* (<- a e) (<- b f))))
               3 (recur rs b (assoc! xs (xs a) (async/<! src)))
               4 (let [x (<- a e)] (async/offer! dst x) (recur x b xs))
               5 (recur rs (if (not= (<- a e) 0) (<- b f) c) xs)
               6 (recur rs (if (=    (<- a e) 0) (<- b f) c) xs)
               7 (recur rs d (assoc! xs (xs c) (if (< (<- a e) (<- b f)) 1 0)))
               8 (recur rs d (assoc! xs (xs c) (if (= (<- a e) (<- b f)) 1 0)))
               9 rs)))
         (list src dst))))
