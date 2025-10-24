(def adjacent [{:x 0 :y -1} {:x -1 :y 0} {:x 1 :y 0} {:x 0 :y 1}])
(def queue clojure.lang.PersistentQueue/EMPTY)
(def reading (juxt :y :x))
(def reading-cmp (fn [a b] (compare (reading a) (reading b))))

(defn add [a]
  (fn [b]
    {:x (+ (:x a) (:x b))
     :y (+ (:y a) (:y b))}))

(defn parse [xs len]
  (reduce-kv
   (fn [rs idx val]
     (let [x (mod idx len) y (quot idx len) k {:x x :y y}]
       (case val
         (\E \G) (update rs :units assoc k {:hp 200 :atk 3 :type val})
         (\#)    (update rs :walls conj k) rs)))
   {:walls (hash-set)
    :units (sorted-map-by reading-cmp)} xs))

(defn game-over? [units]
  (reduce
   (fn [a b]
     (if (not= (:type (val a)) (:type (val b)))
       (reduced false) b)) units))

(defn elves [units]
  (count (filter (comp #{\E} :type val) units)))

(defn path [walls src dst]
  (loop [queue (conj queue [src]) vstd (hash-set) paths []]
    (if (seq queue)
      (let [path (peek queue) next (peek path) rest (pop queue)]
        (cond
          (and (seq paths) (> (count path) (count (peek paths))))
          paths
          (vstd next)
          (recur rest vstd paths)
          (dst next)
          (recur rest (conj vstd next) (conj paths path))
          :else
          (recur
           (into
            rest
            (comp
             (map (add next))
             (remove vstd)
             (remove walls)
             (map (partial conj path)))
            adjacent)
           (conj vstd next)
           paths)))
      paths)))

(defn move [walls units src]
  (let [char (units src)
        locs (update-vals
              (group-by (comp :type val) units)
              (fn [xs] (into (hash-set) (map key) xs)))
        ally (locs (:type char))
        oppo (locs (if (= (:type char) \E) \G \E))
        path (path (into walls ally) src oppo)]
    (if (seq path)
      (let [dst (first (sort-by reading (sequence (map second) path)))]
        (if (units dst) src dst)) src)))

(defn attack [units src]
  (let [char (units src)
        next (sort-by
              (juxt (comp :hp val) (comp reading key))
              (sequence
               (comp
                (map (add src))
                (map (partial find units))
                (remove nil?)
                (remove (comp #{(:type char)} :type val)))
               adjacent))]
    (if (seq next)
      (let [dst (key (first next))]
        (if-let [enemy (units dst)]
          (if (> (:hp enemy) (:atk char))
            (update-in units [dst :hp] - (:atk char))
            (dissoc units dst))
          units))
      units)))

(defn round [walls units]
  (reduce
   (fn [units src]
     (if-let [char (units src)]
       (if (game-over? units)
         (reduced units)
         (let [dst (move walls units src)]
           (if (= src dst)
             (attack units src)
             (attack (assoc (dissoc units src) dst char) dst))))
       units)) units (keys units)))

(defn update-atk [units atk]
  (reduce
   (fn [rs [k v]]
     (if (= (:type v) \E)
       (update rs k assoc :atk atk) rs)) units units))

(defn play [walls units]
  (loop [units units rnd 0]
    (if (game-over? units)
      (* (dec rnd) (transduce (map (comp :hp val)) + units))
      (recur (round walls units) (inc rnd)))))

(defn cheat [walls units]
  (let [initial (elves units)]
    (loop [rs units rnd 0 atk 4]
      (if (game-over? rs)
        (if (= (elves rs) initial)
          (* (dec rnd) (transduce (map (comp :hp val)) + rs))
          (recur units 0 (inc atk)))
        (recur (round walls (update-atk rs atk)) (inc rnd) atk)))))

(let [in (line-seq (java.io.BufferedReader. *in*))
      xs (into [] cat in)
      ln (count in)
      rs (parse xs ln)]
  (println "Part A:" (play  (:walls rs) (:units rs)))
  (println "Part B:" (cheat (:walls rs) (:units rs))))
