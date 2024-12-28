(ns aoc.24.20
  (:require
   [aoc.lib.graph :refer [shortest-path]]
   [aoc.lib.io :refer [get-puzzle-input]]
   [aoc.lib.matrix :refer [char-matrix mcontains? mfind mget v+]]
   [clojure.string :as str]))

(def ^:private dirs {:n [-1 0], :e [0 1], :s [1 0], :w [0 -1]})

(defn part-1 [grid]
  (let [edgefn (fn [p] (for [d (vals dirs),
                             :let [p' (v+ p d)],
                             :when (and (mcontains? grid p') (not= \# (mget grid p')))]
                         [p' 1]))
        start (first (mfind grid #(= \S %)))
        end (first (mfind grid #(= \E %)))
        path (shortest-path start end edgefn)
        path-offsets (into {} (map vector path (range)))
        ->cheats (fn [p] (for [d (vals dirs), :let [p' (v+ p d), p'' (v+ p' d)],
                               :when (and (mcontains? grid p'')
                                          (= \# (mget grid p'))
                                          (contains? path-offsets p'')
                                          (< (+ 2 (path-offsets p)) (path-offsets p'')))]
                           [[p p''] (- (path-offsets p'') (+ 2 (path-offsets p)))]))]
    (->> (mapcat ->cheats path)
         (filter (fn [[_ t]] (<= 100 t)))
         (count))))

(let [grid (char-matrix (str/split-lines (get-puzzle-input 24 20)))]
  (println "Part 1:" (time (part-1 grid))))
