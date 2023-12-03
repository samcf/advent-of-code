(defn numeric? [c] (<= 48 (int c) 57))
(defn part?    [c] (and (char? c) (not= c \.) (not (numeric? c))))
(defn invert   [[k v]] (map (fn [i] {i [k]}) v))

(defn neighbors [cs wd]
  (let [offsets [(- (- wd) 1) (- wd) (+ (- wd) 1) 1 (+ wd 1) wd (- wd 1) -1]]
    (loop [idx 0   ;; current index into the board
           acc []  ;; collection of tuples [x #{idxs}]
           ids #{} ;; indices of parts neighboring the current number
           num nil ;; string of numeric characters so far
           ]
      (if (< idx (count cs))
        (let [val (cs idx)]
          (if (not (numeric? val))
            (recur (inc idx) (if num (conj acc [num ids]) acc) #{} nil)
            (let [parts (into ids (comp (map (partial + idx)) (filter (comp part? (partial get cs)))) offsets)]
              (if (= (mod idx wd) (- wd 1))
                (recur (inc idx) (conj acc [(str num val) parts]) #{} nil)
                (recur (inc idx) acc parts (str num val)))))) acc))))

(def part-numbers-xf
  (comp (filter (comp seq second))
        (map first)
        (map parse-long)))

(def gear-ratio-xf
  (comp (filter (comp #{\*} first))
        (filter (comp #{2} count second))
        (map second)
        (map (partial map parse-long))
        (map (partial apply *))))

(let [in (line-seq (java.io.BufferedReader. *in*))
      cs (into [] cat in)
      xs (neighbors cs (count (first in)))]
  (println "Part A:" (transduce part-numbers-xf + xs))
  (println "Part B:" (transduce
                      (comp (map (juxt (comp cs key) val)) gear-ratio-xf) +
                      (apply merge-with into
                             (sequence cat (map invert xs))))))
