(ns aoc.24.22
  (:require
   [aoc.lib.io :refer [get-puzzle-input]]
   [clojure.math :refer [floor]]
   [clojure.string :as str]))

(defn parse [input]
  (->> (str/split-lines input)
       (map str/trim)
       (map parse-long)))

(defn- mix [x y] (bit-xor x y))
(defn- prune [x] (mod x 16777216))

(def next-secret
  (memoize
   (fn [x]
     (-> x
         (#(prune (mix % (* 64 %))))
         (#(prune (mix % (long (floor (/ % 32))))))
         (#(prune (mix % (* 2048 %))))))))

(defn part-1 [nums]
  (apply + (for [n nums] (nth (iterate next-secret n) 2000))))

(let [nums (parse (get-puzzle-input 24 22))]
  (println "Part 1:" (time (part-1 nums))))
