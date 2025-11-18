(ns aoc.22.15
  (:require
   [aoc.lib.io :refer [get-puzzle-input]]
   [aoc.lib.matrix :refer [v-]]
   [aoc.lib.parsing :refer [parse-longs]]
   [aoc.lib.range-set :as rs]
   [clojure+.hashp]
   [clojure.string :as str]))

(clojure+.hashp/install!)

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

(defn- coverage
  "Given the positions of all sensors and beacons and a row index _y_, calculate all
  _x_ such that _(x,y)_ is covered by at least one sensor and does not contain a beacon.
  Return the positions as a range set, i.e. a seq of ranges of the form [start end)."
  [sensors-and-beacons y]
  (let [; Calculate the covered ranges in row y as a set of ranges of the form [start end).
        covered (->> (for [[[x_s y_s, :as p_s] p_b] sensors-and-beacons
                           :let [d_sb (apply + (map abs (v- p_s p_b)))
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

(defn part-1 [sbs y]
  (rs/count-elements (coverage sbs y)))

(def ^:private test? false)

(let [input (if test? test-input (get-puzzle-input 22 15))
      sbs (parse input)]
  (println "Part 1:" (time (part-1 sbs (if test? 10 2000000)))))

((juxt identity inc) 1)
