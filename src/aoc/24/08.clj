(ns aoc.24.08
  (:require
   [aoc.lib.io :refer [get-puzzle-input]]
   [aoc.lib.matrix :refer [char-matrix mfind mget mcontains? v+ v-]]
   [aoc.lib.seq :refer [all-pairs]]
   [clojure.string :as str]))


(defn- parse [input]
  (let [grid (char-matrix (str/split-lines input))]
    {:grid grid
     :antennas (reduce (fn [acc pos] (update acc (mget grid pos) #(conj % pos)))
                       {}
                       (mfind grid #(not= \. %)))}))


(defn part-1 [{:keys [grid antennas]}]
  (->> (mapcat (fn frequency-positions->antinode [positions]
                 (mapcat (fn pair->antinodes [[p1 p2]] (let [d (v- p2 p1)] [(v- p1 d) (v+ p2 d)]))
                         (all-pairs positions)))
               (vals antennas))
       (set)
       (filter (partial mcontains? grid))
       (count)))


(defn part-2 [{:keys [grid antennas]}]
  (let [on-the-grid? (partial mcontains? grid)]
    (->> (mapcat (fn frequency-positions->antinodes [positions]
                   (mapcat (fn pair->antinodes [[p1 p2]]
                             (let [d (v- p2 p1)]
                               (concat
                                (take-while on-the-grid? (iterate #(v+ % d) p2))
                                (take-while on-the-grid? (iterate #(v- % d) p1)))))
                           (all-pairs positions)))
                 (vals antennas))
         (set)
         (count))))


(let [problem (parse (get-puzzle-input 24 8))]
  (println "Part 1:" (time (part-1 problem)))
  (println "Part 2:" (time (part-2 problem))))
