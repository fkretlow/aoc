(ns aoc.22.09
  (:require [aoc.lib.io :refer [get-puzzle-input]]
            [aoc.lib.matrix :refer [v+ v-]]
            [clojure.math :refer [signum]]
            [clojure.string :as str]))

;; Terms
;; - move:      a vector of the form [dir count], where dir is one of :up, :right, :down, :left
;; - position:  a vector of the form [x y] indicating the current position of a knot on the grid
;; - step:      a vector of the form [dx dy] indicating the difference of two positions

(def ^:private dir->step {:up [0 1], :right [1 0], :down [0 -1], :left [-1 0]})

(defn- parse-moves
  [input]
  (for [move (str/split-lines input)
        :let [[dirstr nstr] (str/split move #"\s")]]
    [(case dirstr
       "U" :up
       "R" :right
       "D" :down
       "L" :left
       (throw (ex-info (str "funny direction" dirstr) {:dir dirstr}))) (parse-long nstr)]))

(defn- ->follower-step
  "Given leader and follower positions, compute the step that the follower will take
  in order to catch up and return it in the form `[dx dy]`."
  [leader follower]
  (let [[dx dy] (v- leader follower)]
    (cond
      ; dy = 0, |dx| > 1 -> move horizontally
      (and (zero? dy) (< 1 (abs dx))) [(signum dx) 0]
      ; dx = 0, |dy| > 1 -> move vertically
      (and (zero? dx) (< 1 (abs dy))) [0 (signum dy)]
      ; |dx| + |dy| > 2 -> move diagonally
      (< 2 (+ (abs dx) (abs dy))) [(signum dx) (signum dy)]
      ; otherwise don't move
      :else [0 0])))

(defn- apply-step
  "Given a sequence of knot positions, move the leader by `leader-step` and simulate
  all the rest of the knots following the leader one by one."
  [[leader & followers] leader-step]
  (let [leader' (v+ leader leader-step)]
    (if-not (seq followers)
      [leader']
      (let [follower-step (->follower-step leader' (first followers))]
        (cons leader' (apply-step followers follower-step))))))

(defn simulate-moves
  "Given a number of knots and moves, simulate how the rope moves and return the number of unique tail positions."
  [n-knots moves]
  (let [dirs (->> moves
                  (map (fn [[step n]] (repeat n step)))
                  (apply concat))]
    (-> (reduce (fn [{:keys [knots], :as state} dir]
                  (let [knots' (apply-step knots (dir->step dir))]
                    (-> state
                        (assoc :knots knots')
                        (update :tail-positions #(conj (or % #{}) (last knots'))))))
                {:knots (repeat n-knots [0 0])}
                dirs)
        :tail-positions
        count)))

(let [input (get-puzzle-input 22 9)
      moves (parse-moves input)]
  (println "Part 1:" (time (simulate-moves 2 moves)))
  (println "Part 2:" (time (simulate-moves 10 moves))))
