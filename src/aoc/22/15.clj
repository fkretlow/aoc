(ns aoc.22.15
  (:require
   [aoc.lib.io :refer [get-puzzle-input]]
   [aoc.lib.math :refer [->line-parameters intersection-of-lines]]
   [aoc.lib.matrix :refer [v-]]
   [aoc.lib.parsing :refer [parse-longs]]
   [aoc.lib.range-set :as rs]
   [clojure+.hashp]
   [clojure.string :as str]))

; See https://adventofcode.com/2022/day/15

(def ^:private test-input (str/trim "
 Sensor at x=2, y=18: closest beacon is at x=-2, y=15
 Sensor at x=9, y=16: closest beacon is at x=10, y=16
 Sensor at x=13, y=2: closest beacon is at x=15, y=3
 Sensor at x=12, y=14: closest beacon is at x=10, y=16
 Sensor at x=10, y=20: closest beacon is at x=10, y=16
 Sensor at x=14, y=17: closest beacon is at x=10, y=16
 Sensor at x=8, y=7: closest beacon is at x=2, y=10
 Sensor at x=2, y=0: closest beacon is at x=2, y=10
 Sensor at x=0, y=11: closest beacon is at x=2, y=10
 Sensor at x=20, y=14: closest beacon is at x=25, y=17
 Sensor at x=17, y=20: closest beacon is at x=21, y=22
 Sensor at x=16, y=7: closest beacon is at x=15, y=3
 Sensor at x=14, y=3: closest beacon is at x=15, y=3
 Sensor at x=20, y=1: closest beacon is at x=15, y=3"))

(defn- parse [input]
  (->> (str/split-lines input)
       (map parse-longs)
       (map (partial partition 2))))

(defn- distance [p q] (apply + (map abs (v- p q))))

(defn- coverage-in-row
  "Given the positions of all sensors and beacons and a row index _y_, calculate all
  _x_ such that _(x,y)_ is covered by at least one sensor and does not contain a beacon.
  Return the positions as a range set, i.e. a seq of ranges of the form [start end)."
  [sensors-and-beacons y]
  (let [; Calculate the covered ranges in row y as a set of ranges of the form [start end).
        covered (->> (for [[[x_s y_s, :as p_s] p_b] sensors-and-beacons
                           :let [d_sb (distance p_s p_b)
                                 d_y (abs (- y y_s))
                                 d_xmax (- d_sb d_y)]
                           :when (<= d_y d_sb)]
                       [[(- x_s d_xmax) (+ x_s d_xmax 1)]])
                     (apply rs/union))
        ; Find all beacons in row y and transform them to a set of ranges of the form [x_b (inc x_b)].
        beacons (->> sensors-and-beacons
                     (map second)
                     (filter (fn [[_ y_b]] (= y y_b)))
                     (map first)
                     (map (juxt identity inc)))]
    ; Return the covered ranges minus the positions of beacons.
    (rs/difference covered beacons)))

(defn part-1 [sensors-and-beacons y]
  (rs/count-elements (coverage-in-row sensors-and-beacons y)))

; For the second part, since there's only a single point where the beacon
; can be, it must either abut the coverage areas of at least two sensors
; or it must be at a corner point of the search area.
; Thus by calculating the pairwise intersections of edges of coverage areas
; we end up with a manageable number of candidate points that we can test
; against all coverage areas.

(defn- ->edges-of-covered-area
  "Given the positions of a sensor and its beacon, return the edges
  hugging the diamond shaped area the sensor is covering in the form
  `{:sw->ne [[left top] [bottom right]], :nw->se [[top right] [left bottom]]}`
  where `top`, `right`, `bottom`, and `left` are the corner points of the
  diamond."
  [[p_s p_b]]
  (let [[x y] p_s
        d (inc (distance p_s p_b)),
        t [x (+ y d)],
        r [(+ x d) y],
        b [x (- y d)],
        l [(- x d) y]]
    {:sw->ne [[l t] [b r]],
     :nw->se [[t r] [l b]]}))

(defn- find-intersection-of-edges
  "Given two edges of the form `[start end]`, return the coordinates
  of their intersection point if it exists."
  [e1 e2]
  (let [[[x_a y_a] [x_b y_b]] e1
        [[x_p y_p] [x_q y_q]] e2
        [x_s y_s, :as s] (apply intersection-of-lines (map ->line-parameters [e1 e2]))]
    (when (and (rs/contains? [[x_a x_b] [x_p x_q]] x_s)
               (rs/contains? [[y_a y_b] [y_p y_q]] y_s))
      s)))

(defn part-2 [sensors-and-beacons c_max]
  (let [; Find all edges that hug covered areas, partitioned by orientation
        {:keys [sw->ne nw->se]} (->> sensors-and-beacons
                                     (map ->edges-of-covered-area)
                                     (apply merge-with into))]
    (->>
     ; Find all intersections of orthogonal edges
     (for [l1 sw->ne, l2 nw->se] (find-intersection-of-edges l1 l2))
     ; Eliminate duplicates and add the corner coordinates of the search area
     (into #{[0 0] [0 c_max] [c_max 0] [c_max c_max]})
     ; Only consider intersections at integer coordinates
     (filter (fn [p] (every? integer? p)))
     ; Restrict to the search area
     (filter (fn [p] (every? #(<= 0 % c_max) p)))
     ; Filter out intersections covered by other sensors
     (filter (fn [p] (not-any? (fn [[p_s p_b]] (<= (distance p_s p) (distance p_s p_b)))
                               sensors-and-beacons)))
     first
     ((fn [[x y]] (+ (* 4000000 x) y))))))

(def ^:private test? false)

(let [input (if test? test-input (get-puzzle-input 22 15))
      sensors-and-beacons (parse input)]
  (println "Part 1:" (time (part-1 sensors-and-beacons (if test? 10 2000000))))
  (println "Part 2:" (time (part-2 sensors-and-beacons (if test? 20 4000000)))))

