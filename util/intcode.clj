(ns intcode (:require [clojure.core.async :as async]))

(defn- fill [xs n]
  (concat (repeat (- n (count xs)) 0) xs))

(defn- split [x]
  (loop [x x rs (list)]
    (if (> x 0) (recur (quot x 10) (conj rs (mod x 10)))
        rs)))

(defn ^:export intcode [xs]
  (let [xs (transient xs)
        <- (fn [idx mode base]
             (case mode
               0 (get xs (get xs idx 0) 0)
               1 (get xs idx 0)
               2 (get xs (+ (get xs idx 0) base) 0)))
        -> (fn [idx mode base]
             (case mode
               0 (get xs idx 0)
               2 (+ (get xs idx 0) base)))
        src (async/chan)
        dst (async/chan)]
    (->> (async/go-loop [xs xs idx 0 off 0 out nil]
           (let [instr (xs idx)
                 [g f e _ opcode] (fill (split instr) 5)
                 a (+ idx 1)
                 b (+ idx 2)
                 c (+ idx 3)
                 d (+ idx 4)]
             (if (= instr 99) out
                 (case opcode
                   1 (recur (assoc! xs (-> c g off) (+ (<- a e off) (<- b f off))) d off out)
                   2 (recur (assoc! xs (-> c g off) (* (<- a e off) (<- b f off))) d off out)
                   3 (recur (assoc! xs (-> a e off) (async/<! src)) b off out)
                   4 (let [x (<- a e off)] (async/>! dst x) (recur xs b off x))
                   5 (recur xs (if (not= (<- a e off) 0) (<- b f off) c) off out)
                   6 (recur xs (if (=    (<- a e off) 0) (<- b f off) c) off out)
                   7 (recur (assoc! xs (-> c g off) (if (< (<- a e off) (<- b f off)) 1 0)) d off out)
                   8 (recur (assoc! xs (-> c g off) (if (= (<- a e off) (<- b f off)) 1 0)) d off out)
                   9 (recur xs b (+ (<- a e off) off) out)))))
         (list src dst))))
