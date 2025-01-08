(ns aoc.15.01
  (:require
   [aoc.lib.io :refer [get-puzzle-input]]))

(def ^:private ->step {\( 1, \) -1})

(defn part-1 [input]
  (reduce + (map ->step input)))

(defn part-2 [input]
  (->> (map ->step input)
       (reductions +)
       (map-indexed vector)
       (drop-while (fn [[_ floor]] (not= -1 floor)))
       (ffirst)
       (inc)))

(let [input (get-puzzle-input 15 1)]
  (println "Part 1:" (time (part-1 input)))
  (println "Part 2:" (time (part-2 input))))

