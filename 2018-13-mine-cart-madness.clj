(require '[clojure.string :refer [join]])

(def position (comp (juxt :y :x) key))
(def rotation {0 1 1 2 2 0})

(def cross
  {[\^ 0] \< [\^ 1] \^ [\^ 2] \>
   [\> 0] \^ [\> 1] \> [\> 2] \v
   [\v 0] \> [\v 1] \v [\v 2] \<
   [\< 0] \v [\< 1] \< [\< 2] \^})

(def corner
  {[\^ \\] \< [\> \\] \v [\v \\] \> [\< \\] \^
   [\^ \/] \> [\> \/] \^ [\v \/] \< [\< \/] \v})

(def movement
  {\^ {:x 0 :y -1}
   \> {:x 1 :y 0}
   \v {:x 0 :y 1}
   \< {:x -1 :y 0}})

(defn turn [cart val]
  (let [dir (:dir cart) rot (:rot cart)]
    (case val
      \+ (assoc cart :dir (cross [dir rot]) :rot (rotation rot))
      \\ (assoc cart :dir (corner [dir val]))
      \/ (assoc cart :dir (corner [dir val]))
      cart)))

(defn move [a cart]
  (let [b (movement (:dir cart))]
    {:x (+ (:x a) (:x b))
     :y (+ (:y a) (:y b))}))

(defn parse [xs len]
  (reduce-kv
   (fn [ret idx val]
     (let [x (mod idx len) y (quot idx len) k {:x x :y y}]
       (case val
         (\^ \> \v \<) (update ret :carts assoc k {:dir val :rot 0})
         (\\ \/ \+) (update ret :track assoc k val)
         ret))) {:track {} :carts {}} xs))

(defn tick [ts cs halt?]
  (reduce-kv
   (fn [cs k v]
     (let [j (turn v (ts k)) u (move k j)]
       (if (cs k)
         (if (cs u)
           (if halt?
             (reduced {u j})
             (dissoc cs k u))
           (assoc (dissoc cs k) u j))
         cs))) cs (sort-by position cs)))

(defn run [ts cs halt?]
  (let [cs (tick ts cs halt?)]
    (if (= (count cs) 1)
      (join "," (vals (key (first cs))))
      (recur ts cs halt?))))

(let [in (line-seq (java.io.BufferedReader. *in*))
      ln (count (first in))
      rs (parse (into [] cat in) ln)]
  (println "Part A:" (run (:track rs) (:carts rs) true))
  (println "Part B:" (run (:track rs) (:carts rs) false)))
