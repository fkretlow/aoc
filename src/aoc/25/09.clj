(ns aoc.25.09
  (:require
   [aoc.lib.io :refer [get-puzzle-input]]
   [aoc.lib.parsing :refer [parse-longs]]
   [clojure.string :as str]))

(def ^:private test-input (str/trim "
7,1
11,1
11,7
9,7
9,5
2,5
2,3
7,3"))

(defn- parse [input]
  (->> (str/split-lines input)
       (map parse-longs)))

(defn- area [[x1 y1] [x2 y2]]
  (* (inc (abs (- x2 x1)))
     (inc (abs (- y2 y1)))))

(defn part-1 [points]
  (loop [a_max 0, [p & qs] points]
    (if (empty? qs)
      a_max
      (let [a_max' (apply max (for [q qs] (area p q)))]
        (recur (max a_max a_max') qs)))))

(def ^:private test? false)

(let [input (if test? test-input (get-puzzle-input 25 9))
      points (parse input)]
  (println "Part 1:" (time (part-1 points))))
