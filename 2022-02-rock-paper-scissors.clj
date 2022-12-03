(def solve-a {"A X" 4 "B X" 1 "C X" 7 "A Y" 8 "B Y" 5 "C Y" 2 "A Z" 3 "B Z" 9 "C Z" 6})
(def solve-b {"A X" 3 "B X" 1 "C X" 2 "A Y" 4 "B Y" 5 "C Y" 6 "A Z" 8 "B Z" 9 "C Z" 7})

(let [lns (line-seq (java.io.BufferedReader. *in*))]
  (println "Part A:" (transduce (map solve-a) + lns))
  (println "Part B:" (transduce (map solve-b) + lns)))
