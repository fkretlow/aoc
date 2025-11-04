(ns aoc.15.09
  (:require [aoc.lib.io :refer [get-puzzle-input]]
            [aoc.lib.matrix :refer [v+ v-]]
            [clojure.string :as str]))

(defn sign
  [x]
  (cond (= 0 x) 0
        (< x 0) -1
        :else 1))

(defn parse-moves
  [input]
  (for [move (str/split-lines input)
        :let [[dir nstr] (str/split move #"\s")]]
    [(case dir
       "U" :up
       "R" :right
       "D" :down
       "L" :left
       (throw (ex-info (str "funny direction" dir) {:dir dir})))
     (parse-long nstr)]))

(defn- follow-tail
  "Given head and tail positions, make the tail follow if necessary and return its updated position."
  [head tail]
  (let [[dx dy] (v- head tail)
        d_tail (cond
                 ; dy = 0, |dx| > 1 -> move horizontally
                 (and (zero? dy) (< 1 (abs dx))) [(sign dx) 0]
                 ; dx = 0, |dy| > 1 -> move vertically
                 (and (zero? dx) (< 1 (abs dy))) [0 (sign dy)]
                 ; |dx| + |dy| > 2 -> move diagonally
                 (< 2 (+ (abs dx) (abs dy))) [(sign dx) (sign dy)]
                 ; otherwise don't move
                 :else [0 0])]
    (v+ tail d_tail)))

(defn- move->steps
  [[dir n]]
  (repeat n
          (case dir
            :up [0 1]
            :right [1 0]
            :down [0 -1]
            :left [-1 0])))

(defn step
  [{:keys [head tail], :as state} d_head]
  (let [head' (v+ head d_head)
        tail' (follow-tail head' tail)]
    (-> state
        (assoc :head head')
        (assoc :tail tail')
        (update :tail-positions #(conj % tail')))))

(defn part-1
  [input]
  (let [moves (parse-moves input)
        steps (->> moves
                   (map move->steps)
                   (apply concat))]
    (-> (reduce step {:head [0 0], :tail [0 0], :tail-positions #{[0 0]}} steps)
        :tail-positions
        count)))

(let [input (get-puzzle-input 15 9)] (println "Part 1:" (time (part-1 input))))
