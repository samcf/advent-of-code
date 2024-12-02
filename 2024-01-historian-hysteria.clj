(require '[net.cgrand.xforms.io :as io]
         '[net.cgrand.xforms :as x])

(def xf (comp (mapcat (fn [ln] (re-seq #"\d+" ln)))
              (map parse-long)
              (x/multiplex {:xs (take-nth 2) :ys (comp (drop 1) (take-nth 2))})
              (x/by-key (comp (x/sort) (x/into [])))))

(let [{:keys [xs ys]} (into {} xf (io/lines-in *in*))]
  (println "Part A:" (reduce + (map (comp abs -) xs ys)))
  (println "Part B:" (let [frqs (frequencies ys)]
                       (transduce (map (fn [x] (* (get frqs x 0) x))) + xs))))
