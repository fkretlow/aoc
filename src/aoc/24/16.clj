(ns aoc.24.16
  (:require
   [aoc.lib.graph :refer [all-min-distances]]
   [aoc.lib.io :refer [get-puzzle-input]]
   [aoc.lib.matrix :refer [char-matrix mfind mget v+]]
   [aoc.lib.seq :refer [keys-with-min-values]]
   [clojure.string :as str]))

;; Modeling movement through the grid as a graph whose vertices are pairs of the form (position, direction),
;; this boils down to a simple shortest path problem. The first part is easily solved with Dijkstra.
;;
;; For the second part, we're interested in all vertices that belong to any minimal path from the start tile
;; to the end tile. Interestingly, we don't need to enumerate all shortest paths for this. The knowledge of
;; minimal distances from the starting vertex that we collected with Dijkstra in the first part is enough:
;;
;; Let s be the starting vertex, and let e be the end vertex. We make use of the fact that for any vertex v,
;; if v is on a minimal path from s to e, any minimal path from s to v can be used to construct a minimal
;; path from s to e, likewise for any minimal path from v to e. Thus if v is on a minimal path from s to e,
;; then any neighbor u of v such that d(s,u) + d(u,v) = d(s,v) is on a minimal path from s to e. Starting
;; with all end vertices, we can simply walk back, collecting vertices with correct distances on the way.

(def ^:private dir->vector {:n [-1 0], :e [0 1], :s [1 0], :w [0 -1]})

(defn- walk-forward [[pos dir]] [(v+ pos (dir->vector dir)) dir])
(defn- turn-left [[pos dir]] [pos (case dir :n :w, :e :n, :s :e, :w :s)])
(defn- turn-right [[pos dir]] [pos (case dir :n :e, :e :s, :s :w, :w :n)])
(defn- turn-back [[pos dir]] [pos (case dir :n :s, :e :w, :s :n, :w :e)])

(def ^:private steps [[walk-forward 1] [turn-left 1000] [turn-right 1000]])

(defn- edgefn
  "Given a vertex `v = [pos dir]`, return all vertices that we con reach in one step as a seq
  of pairs of vertices and step costs."
  [grid v]
  (->> steps
       (map (fn [[f d]] [(f v) d]))
       (filter (fn [[[pos]]] (not= \# (mget grid pos))))))

(defn- reverted-edgefn
  "Like `edgefn`, but walking backwards."
  [grid v]
  (->> steps
       (map (fn [[f d]] [(turn-back (f (turn-back v))) d]))
       (filter (fn [[[pos]]] (not= \# (mget grid pos))))))

(defn part-1 [grid]
  (let [start (first (mfind grid #(= \S %)))
        end (first (mfind grid #(= \E %)))
        min-distances (all-min-distances [start :e] (partial edgefn grid))]
    (->> min-distances
         (filter (fn [[[pos]]] (= pos end)))
         (map second)
         (apply min))))

(defn- part-2 [grid]
  (let [start (first (mfind grid #(= \S %)))
        end (first (mfind grid #(= \E %)))
        min-distances (all-min-distances [start :e] (partial edgefn grid))]
    (loop [Q (set (keys-with-min-values (filter (fn [[[pos]]] (= end pos)) min-distances)))
           V #{}]
      (if (empty? Q)
        (count (set (map first V)))
        (let [v (first Q)
              Q' (disj Q v)
              d_0v (get min-distances v)]
          (recur (reduce (fn [Q [u d_vu]]
                           (let [d_0u (get min-distances u)]
                             (cond-> Q (and (= d_0v (+ d_0u d_vu)) (not (V u)) (not (Q u))) (conj u))))
                         Q'
                         (reverted-edgefn grid v))
                 (conj V v)))))))

(let [grid (-> (get-puzzle-input 24 16)
               str/split-lines
               char-matrix)]
  (println "Part 1:" (time (part-1 grid)))
  (println "Part 2:" (time (part-2 grid))))
