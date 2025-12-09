(ns aoc.25.07
  (:require
   [aoc.lib.io :refer [get-puzzle-input]]
   [aoc.lib.matrix :refer [char-matrix find-indices mshape]]
   [aoc.lib.seq :refer [mapvals]]
   [clojure.string :as str]))

(def ^:private test-input (str/trim "
.......S.......
...............
.......^.......
...............
......^.^......
...............
.....^.^.^.....
...............
....^.^...^....
...............
...^.^...^.^...
...............
..^...^.....^..
...............
.^.^.^.^.^...^.
..............."))

(defn- parse [input]
  (let [grid (char-matrix (str/split-lines input)),
        [height] (mshape grid)
        ; a map from row indices to sets of column indices of splitters
        splitters (->> (find-indices #(= \^ %) grid)
                       (group-by first)
                       (mapvals #(into #{} (map second %)))),
        start (first (find-indices #(= \S %) grid))]
    {:start start, :splitters splitters, :height height}))

(defn- step [{:keys [splitters splits row beams], :as state}]
  (let [row' (inc row),
        splitters-in-row (get splitters row'),
        [splits' beams']
        (loop [beams beams, splits' splits, beams' #{}]
          (if (empty? beams)
            [splits' beams']
            (let [b (first beams)]
              (if (contains? splitters-in-row b)
                (recur (rest beams) (inc splits') (conj beams' (dec b) (inc b)))
                (recur (rest beams) splits' (conj beams' b))))))]
    (-> state (assoc :row row') (assoc :splits splits') (assoc :beams beams'))))

(defn part-1 [{:keys [start splitters height]}]
  (let [state {:splitters splitters, :splits 0, :row (first start), :beams #{(second start)}}]
    (->> (iterate step state)
         (drop-while (fn [{row :row}] (< row height)))
         first
         :splits)))

(def ^:private test? false)

(let [input (if test? test-input (get-puzzle-input 25 7))
      parsed (parse input)]
  (println "Part 1:" (time (part-1 parsed))))
