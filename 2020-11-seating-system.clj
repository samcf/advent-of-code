(defn to-index [width x y]
  (+ (* y width) x))

(defn neighbors [board width height distance index]
  (for [offv [[1 1] [1 0] [1 -1] [0 1] [0 -1] [-1 1] [-1 0] [-1 -1]]
        step (range 1 (inc distance))
        :let [[ox oy] offv
              sx (mod index width)
              sy (int (/ index width))
              bx (+ sx (* step ox))
              by (+ sy (* step oy))]
        :while (and (< -1 bx width) (< -1 by height))
        :let [ax (+ sx (* (dec step) ox))
              ay (+ sy (* (dec step) oy))
              av (board (to-index width ax ay))
              bv (board (to-index width bx by))
              sv (board index)]
        :while (or (= step 1) (= av \.))
        :when  (and (or (= sv \L) (= sv \#))
                    (or (= bv \L) (= bv \#)))]
    (to-index width bx by)))

(defn step-xf [board threshold]
  (comp (map-indexed vector)
        (map (fn [[idx adj]]
               (let [chr (board idx)
                     frq (frequencies (map board adj))]
                 (cond (and (= chr \L) (=  (get frq \# 0) 0)) \#
                       (and (= chr \#) (>= (get frq \# 0) threshold)) \L
                       :else chr))))))

(defn solve [board seats threshold]
  (loop [prev board]
    (let [next (into [] (step-xf prev threshold) seats)]
      (if (not= prev next)
        (recur next)
        ((frequencies next) \#)))))

(let [input (line-seq (java.io.BufferedReader. *in*))
      width (count (first input))
      heigt (count input)
      board (into [] cat input)
      seats (fn [ds] (sequence (map (fn [ix] (neighbors board width heigt ds ix)))
                               (range (* width heigt))))]
  (println "Part A:" (solve board (seats 1) 4))
  (println "Part B:" (solve board (seats (max width heigt)) 5)))
