(ns aoc.24.18
  (:require
   [aoc.lib.graph :refer [all-min-distances connected-components]]
   [aoc.lib.io :refer [get-puzzle-input]]
   [aoc.lib.matrix :refer [char-matrix mcontains? mfind mget mset v+]]
   [aoc.lib.parsing :refer [parse-longs]]
   [clojure.string :as str]
   [jordanlewis.data.union-find :as uf]))

(def ^:private size 71)
(def ^:private n 1024)

(defn- parse-positions [input]
  (->> (str/split-lines input) (map parse-longs) (map reverse)))

(defn- simulate-falling-bytes [grid ps]
  (reduce (fn [grid p] (mset grid p \#)) grid ps))

(defn- edgefn [M p]
  (for [d [[-1 0] [0 1] [1 0] [0 -1]],
        :let [p' (v+ p d)]
        :when (and (mcontains? M p') (not= \# (mget M p')))]
    [p' 1]))

(defn part-1 [ps]
  (let [grid (-> (repeat size (apply str (repeat size \.)))
                 (char-matrix)
                 (simulate-falling-bytes (take n ps))),
        start [0 0],
        end [(dec size) (dec size)],
        min-distances (all-min-distances start (partial edgefn grid))]
    (get min-distances end)))

(defn part-2 [ps]
  (let [start [0 0],
        end [(dec size) (dec size)]]
    (loop [grid (-> (repeat size (apply str (repeat size \.)))
                    (char-matrix)
                    (simulate-falling-bytes ps)),
           dsf (connected-components (mfind grid #(= \. %)) (partial edgefn grid)),
           Q (vec ps)]
      (let [v (peek Q),
            Q' (pop Q),
            grid' (mset grid v \.),
            dsf' (reduce (fn [dsf [u]] (uf/union dsf v u)) (conj dsf v) (edgefn grid' v))]
        (if (= (dsf' start) (dsf' end))
          v
          (recur grid' dsf' Q'))))))

(let [ps (parse-positions (get-puzzle-input 24 18))]
  (println "Part 1:" (time (part-1 ps)))
  (println "Part 2:" (time (part-2 ps))))

