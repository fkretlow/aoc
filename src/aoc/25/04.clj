(ns aoc.25.04
  (:require
   [aoc.lib.io :refer [get-puzzle-input]]
   [aoc.lib.matrix :as m]
   [clojure.data.priority-map :as pm]
   [clojure.string :as str]))


(def ^:private test-input (str/trim "
..@@.@@@@.
@@@.@.@.@@
@@@@@.@.@@
@.@@@@..@.
@@.@@@@.@@
.@@@@@@@.@
.@.@.@.@@@
@.@@@.@@@@
.@@@@@@@@.
@.@.@@@.@."))

(defn- parse [input]
  (m/char-matrix (str/split-lines input)))

(defn- count-adjacent
  "Given the grid indicating paper rolls and empty space, find the
  number of adjacent paper rolls for each space and return it as a
  priority map from index pairs to counts of adjacent paper rolls."
  [grid]
  (let [; the positions of @ on the grid
        ps_roll (into #{} (m/find-indices #(= \@ %) grid))
        ; a priority map from positions of @ on the grid to the number of
        ; adjacent @s, initialized to 0 for all values
        p->n_adj (into (pm/priority-map) (map vector ps_roll (repeat 0)))]
    (reduce
     (fn [p->n_adj p] (update p->n_adj p inc))
     p->n_adj
     (apply concat (map #(->> (m/madj grid %) (filter ps_roll))
                        ps_roll)))))

(defn part-1 [grid]
  (->> (count-adjacent grid)
       vals
       (filter #(< % 4))
       count))

(defn part-2 [grid]
  (let [p->n_adj (count-adjacent grid)
        p->n_adj' (loop [p->n_adj p->n_adj]
                    (let [[p n] (first p->n_adj)]
                      (if (or (empty? p->n_adj) (<= 4 n))
                        p->n_adj
                        (recur (reduce
                                (fn [p->n_adj' p']
                                  (if (contains? p->n_adj' p')
                                    (update p->n_adj' p' dec)
                                    p->n_adj'))
                                (dissoc p->n_adj p)
                                (m/madj grid p))))))]
    (- (count p->n_adj) (count p->n_adj'))))

(def ^:private test? false)

(let [input (if test? test-input (get-puzzle-input 25 4))
      grid (parse input)]
  (println "Part 1:" (time (part-1 grid)))
  (println "Part 2:" (time (part-2 grid))))
