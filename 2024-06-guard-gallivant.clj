(require '[clojure.string :as s])

(def turn {[0 -1] [1 0] [1 0] [0 1] [0 1] [-1 0] [-1 0] [0 -1]})

(defn patrol [xs idx len obs]
  (loop [curr idx vst #{} [x y :as dir] [0 -1]]
    (if (contains? vst [curr x y]) nil
        (let [seen (conj vst [curr x y])
              next (+ curr (+ x (* y len)))
              char (get xs next)]
          (cond (= next obs) (recur curr seen (turn dir))
                (= char \#)  (recur curr seen (turn dir))
                (= char \^)  (recur next seen dir)
                (= char \.)  (recur next seen dir)
                :else seen)))))

(let [in (line-seq (java.io.BufferedReader. *in*))
      ln (inc (count (first in)))
      xs (apply str (mapcat (fn [ln] (str ln \X)) in))
      st (s/index-of xs \^)
      vs (into #{} (map first) (patrol xs st ln nil))]
  (println "Part A:" (count vs))
  (println "Part B:" (count (remove (fn [idx] (patrol xs st ln idx)) vs))))
