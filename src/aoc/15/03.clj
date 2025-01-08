(ns aoc.15.03
  (:require
   [aoc.lib.io :refer [get-puzzle-input]]
   [aoc.lib.matrix :refer [v+]]))

(def ^:private ->dir {\^ [-1 0], \> [0 1], \v [1 0], \< [0 -1]})

(defn part-1 [steps]
  (->> (map ->dir steps)
       (reductions v+ [0 0])
       (distinct)
       (count)))

(defn part-2 [steps]
  (->> (map ->dir steps)
       (partition 2)
       (reductions (fn [[p1 p2] [d1 d2]] [(v+ p1 d1) (v+ p2 d2)]) [[0 0] [0 0]])
       (apply concat)
       (distinct)
       (count)))

(let [steps (get-puzzle-input 15 3)]
  (println "Part 1" (time (part-1 steps)))
  (println "Part 2" (time (part-2 steps))))
