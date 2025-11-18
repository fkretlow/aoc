(ns aoc.22.14
  (:require
   [aoc.lib.io :refer [get-puzzle-input]]
   [aoc.lib.matrix :refer [v+]]
   [aoc.lib.parsing :refer [parse-longs]]
   [clojure.set :as set]
   [clojure.string :as str]))

;; See https://adventofcode.com/2022/day/14

(def ^:private test-input (str/trim "
 498,4 -> 498,6 -> 496,6
 503,4 -> 502,4 -> 502,9 -> 494,9"))

(defn ->line
  "Given the endpoints of a line (both inclusive), return the set of all points on that line."
  [[x1 y1] [x2 y2]]
  (cond
    (and (= x1 x2) (not= y1 y2)) (for [y (range (min y1 y2) (inc (max y1 y2)))] [x1 y])
    (and (not= x1 x2) (= y1 y2)) (for [x (range (min x1 x2) (inc (max x1 x2)))] [x y1])
    :else (throw (ex-info (format "not a straight line: %d,%d -> %d,%d" x1 y1 x2 y2) {}))))

(defn- parse-rock-structure
  "Parse the given string as a rock structure. Return the set of all points occupied by the structure."
  [s]
  (->> (parse-longs s)
       (partition 2)
       (partition 2 1)
       (map (partial apply ->line))
       (reduce into #{})))

(def ^:private sand-start-pos [500,0])

(defn- simulate-single-sand-unit
  "Simulate a single unit of sand falling down from its `start` position.
  Return the position where it finally comes to rest."
  [is-blocked? is-abyss?]
  (loop [p sand-start-pos]
    (when-not (is-abyss? p)
      (if-let [p' (->> [[0 1] [-1 1] [1 1]]
                       (map #(v+ p %))
                       (filter (complement is-blocked?))
                       (first))]
        (recur p')
        p))))

(defn part-1 [is-rock?]
  (let [y_max (->> is-rock? (map second) (apply max))
        is-abyss? (fn [[_x y]] (< y_max y))]
    (loop [n 0, is-sand? #{}]
      (if-let [new-sand-pos (simulate-single-sand-unit (some-fn is-rock? is-sand?) is-abyss?)]
        (recur (inc n) (conj is-sand? new-sand-pos))
        n))))

(defn part-2 [is-rock?]
  (let [y_max (->> is-rock? (map second) (apply max))
        is-rock?' (fn [[_x y, :as p]] (or (is-rock? p) (= y (+ y_max 2))))]
    (loop [n 0, is-sand? #{}]
      (if (is-sand? sand-start-pos)
        n
        (let [new-sand-pos (simulate-single-sand-unit (some-fn is-rock?' is-sand?) (constantly false))]
          (recur (inc n) (conj is-sand? new-sand-pos)))))))

(def ^:private test? false)

(let [input (if test? test-input (get-puzzle-input 22 14))
      is-rock? (->> (str/split-lines input)
                    (map parse-rock-structure)
                    (apply set/union))]
  (println "Part 1:" (time (part-1 is-rock?)))
  (println "Part 2:" (time (part-2 is-rock?))))
