(require '[clojure.string :refer [replace join split]])

(def policy
  {"byr" #(<= 1920 (Integer. %) 2002)
   "iyr" #(<= 2010 (Integer. %) 2020)
   "eyr" #(<= 2020 (Integer. %) 2030)
   "ecl" #(contains? #{"amb" "blu" "brn" "gry" "grn" "hzl" "oth"} %)
   "hcl" #(re-matches #"\#[0-9a-z]{6}" %)
   "pid" #(re-matches #"[0-9]{9}" %)
   "hgt" #(condp re-find %
            #"(\d+)in" :>> (fn [[_ h]] (<= 59  (Integer. h) 76))
            #"(\d+)cm" :>> (fn [[_ h]] (<= 150 (Integer. h) 193))
            false)})

(defn valid-a? [passport]
  (= (into #{} (keys passport)) #{"byr" "iyr" "eyr" "hgt" "hcl" "ecl" "pid"}))

(defn valid-b? [passport]
  (every? #((policy %) (passport %)) (keys passport)))

(let [in (line-seq (java.io.BufferedReader. *in*))
      xf (comp (partition-by #(= % ""))
               (filter       #(not= % '("")))
               (map          #(join " " %))
               (map          #(replace % #":" " "))
               (map          #(split % #" "))
               (map          #(apply hash-map %))
               (map          #(dissoc % "cid"))
               (filter       valid-a?))
      xs (sequence xf in)]
  (println "Part A:" (count xs))
  (println "Part B:" (count (filter valid-b? xs))))
