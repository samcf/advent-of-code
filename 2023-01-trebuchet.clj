(def re-parse-a #"(\d)")
(def re-parse-b #"(?=(one|two|three|four|five|six|seven|eight|nine|\d))")

(def dict {"one" 1 "two" 2 "three" 3 "four" 4 "five" 5 "six" 6 "seven" 7 "eight" 8 "nine" 9
           "1"   1 "2"   2 "3"     3 "4"    4 "5"    5 "6"   6 "7"     7 "8"     8 "9"    9})

(let [in (line-seq (java.io.BufferedReader. *in*))
      xf (fn [re]
           (comp (map (fn [ln] (re-seq re ln)))
                 (map (fn [xs] (map (comp dict second) xs)))
                 (map (juxt first last))
                 (map (fn [[a b]] (str a b)))
                 (map parse-long)))]
  (println "Part A:" (transduce (xf re-parse-a) + in))
  (println "Part B:" (transduce (xf re-parse-b) + in)))
