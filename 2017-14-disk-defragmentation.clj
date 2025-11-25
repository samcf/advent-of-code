(require '[knot :as knot])

(def queue clojure.lang.PersistentQueue/EMPTY)

(def binary
  (map
   (fn [c]
     (.replace
      (format "%4s" (Integer/toBinaryString (Character/digit c 16)))
      " " "0"))))

(defn encode [s]
  (transduce (comp knot/encode binary) str (knot/create s)))

(defn indices [s]
  (comp
   (map (fn [r] (str s "-" r)))
   (mapcat encode)
   (map-indexed (fn [k v] (when (= v \1) k)))
   (filter identity)))

(defn neighbors [idx]
  (let [col (mod idx 128)]
    (cond-> []
      (> col 0)     (conj (dec idx))
      (< col 127)   (conj (inc idx))
      (> idx 127)   (conj (- idx 128))
      (< idx 16256) (conj (+ idx 128)))))

(defn regions [xs]
  (loop [next (conj queue (first xs)) vstd (hash-set) sum 1]
    (cond
      (seq next)
      (let [xfr (comp (remove vstd) (filter xs))
            idx (peek next)]
        (recur (into (pop next) (sequence xfr (neighbors idx))) (conj vstd idx) sum))
      (not= xs vstd)
      (recur (conj next (first (remove vstd xs))) vstd (inc sum))
      :else sum)))

(let [in (first (line-seq (java.io.BufferedReader. *in*)))
      xs (into (hash-set) (indices in) (range 128))]
  (println "Part A:" (count xs))
  (println "Part B:" (regions xs)))
