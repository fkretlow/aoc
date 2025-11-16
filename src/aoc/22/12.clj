(ns aoc.22.12
  (:require
   [aoc.lib.graph :refer [all-min-distances shortest-path]]
   [aoc.lib.io :refer [get-puzzle-input]]
   [aoc.lib.matrix :refer [char-matrix mcontains? mfind mget v+]]
   [clojure.string :as str]))

;; See https://adventofcode.com/2022/day/12

(def ^:private test-input (str/trim "
Sabqponm
abcryxxl
accszExk
acctuvwj
abdefghi
"))

(defn- height [c]
  (case c \S (int \a), \E (int \z), (int c)))

(defn- is-walkable? [grid p p']
  (let [h_p (height (mget grid p)),
        h_p' (height (mget grid p'))]
    (<= (- h_p' h_p) 1)))

(defn- edgefn [grid is-edge? p]
  (->> [[-1 0] [0 1] [1 0] [0 -1]]
       (map #(v+ p %))
       (filter (partial mcontains? grid))
       (filter (partial is-edge? grid p))
       (map #(-> [% 1]))))

(defn part-1 [grid]
  (let [start (first (mfind grid #(= \S %)))
        end (first (mfind grid #(= \E %)))]
    (->> (shortest-path start end (partial edgefn grid is-walkable?))
         count
         dec)))

(defn- is-walkable-backwards? [grid p p']
  (let [h_p (height (mget grid p))
        h_p' (height (mget grid p'))]
    (>= (- h_p' h_p) -1)))

(defn part-2 [grid]
  (let [end (first (mfind grid #(= \E %)))
        min-distances (all-min-distances end (partial edgefn grid is-walkable-backwards?))]
    (->> min-distances
         (filter (fn [[p _dist]] (let [c (mget grid p)] (or (= \S c) (= \a c)))))
         (map second)
         sort
         first)))

(def ^:private test? false)

(let [input (if test? test-input (get-puzzle-input 22 12))
      grid (char-matrix (str/split-lines input))]
  (println "Part 1:" (time (part-1 grid)))
  (println "Part 2:" (time (part-2 grid))))

