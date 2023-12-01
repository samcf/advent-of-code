(require '[clojure.string :refer [index-of last-index-of]])

(def dict {"one" 1 "two" 2 "three" 3 "four" 4 "five" 5 "six" 6 "seven" 7 "eight" 8 "nine" 9
           "1"   1 "2"   2 "3"     3 "4"    4 "5"    5 "6"   6 "7"     7 "8"     8 "9"    9})

(defn matches [key-fn]
  (into (sorted-map)
        (comp (map (juxt key-fn identity))
              (filter (comp some? first)))
        (keys dict)))

(defn pair [ln]
  [(val (first (matches (partial index-of ln))))
   (val (last  (matches (partial last-index-of ln))))])

(let [in (line-seq (java.io.BufferedReader. *in*))
      xf (comp (map (partial map dict))
               (map (juxt first last))
               (map (partial apply str))
               (map #(Integer. %)))]
  (println "Part A:" (transduce (comp (map (partial re-seq #"\d")) xf) + in))
  (println "Part B:" (transduce (comp (map pair) xf) + in)))
