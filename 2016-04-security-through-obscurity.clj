(defn parse [ln]
  (let [[hash id & name] (reverse (re-seq #"\w+" ln))]
    {:id (parse-long id)
     :name (apply str name)
     :hash (seq hash)}))

(defn rotate [n c]
  (char (+ (mod (+ (int c) n -97) 26) 97)))

(defn valid? [room]
  (->>
   (frequencies (:name room))
   (sort-by val >)
   (take 5)
   (map first)
   (= (:hash room))))

(defn decrypt [room]
  (transduce
   (map (partial rotate (:id room))) str
   (:name room)))

(let [in (line-seq (java.io.BufferedReader. *in*))
      xf (fn [f] (comp (map parse) (filter valid?) (filter f) (map :id)))]
  (println "Part A:" (transduce (xf (constantly true)) + in))
  (println "Part B:" (first (sequence (xf (comp #{"storageobjectnorthpole"} decrypt)) in))))
