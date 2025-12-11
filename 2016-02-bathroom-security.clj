(def offset {\U -7 \R 1 \D 7 \L -1})

(def keys-a
  [\. \. \. \. \. \. \.
   \. \. \. \. \. \. \.
   \. \. \1 \2 \3 \. \.
   \. \. \4 \5 \6 \. \.
   \. \. \7 \8 \9 \. \.
   \. \. \. \. \. \. \.
   \. \. \. \. \. \. \.])

(def keys-b
  [\. \. \. \. \. \. \.
   \. \. \. \1 \. \. \.
   \. \. \2 \3 \4 \. \.
   \. \5 \6 \7 \8 \9 \.
   \. \. \A \B \C \. \.
   \. \. \. \D \. \. \.
   \. \. \. \. \. \. \.])

(defn move [keys]
  (fn [idx ins]
    (let [nxt (+ idx (offset ins))]
      (if (= (keys nxt) \.) idx nxt))))

(defn code [keys]
  (let [f (move keys)]
    (fn
      ([]    [(.indexOf keys \5)])
      ([r]   (apply str (map keys (rest r))))
      ([r s] (conj r (reduce f (peek r) s))))))

(let [in (line-seq (java.io.BufferedReader. *in*))]
  (println "Part A:" (transduce (map seq) (code keys-a) in))
  (println "Part B:" (transduce (map seq) (code keys-b) in)))
