(ns aoc.22.10
  (:require [aoc.lib.io :refer [get-puzzle-input]]
            [clojure.string :as str]))

;; See https://adventofcode.com/2022/day/10

(defn- parse-instruction
  [s]
  (let [[op ?arg] (str/split s #" ")]
    (case op
      "noop" [:noop]
      "addx" [:addx
              (or (and (some? ?arg) (parse-long ?arg))
                  (throw (ex-info (format "cannot parse number '%s' in instruction '%s'" ?arg s) {:arg ?arg})))]
      (throw (ex-info (format "cannot parse instruction '%s'" s) {:instruction s})))))

(defn- instruction->dxs
  "Transform an instruction into end-of-cycle register changes, one for each
  cycle the instruction lasts."
  [[op ?V]]
  (case op
    :noop [0]
    :addx [0 ?V]))

(defn part-1
  [instructions]
  (->> (mapcat instruction->dxs instructions)
       (reductions + 1)
       (map-indexed (fn [i X] [(inc i) X]))
       (filter (fn [[cycle]] (= 20 (mod cycle 40))))
       (map (fn [[cycle X]] (* cycle X)))
       (apply +)))

(defn part-2
  [instructions]
  (->> (mapcat instruction->dxs instructions)
       (reductions + 1)
       (map-indexed (fn [i X] (if (<= (dec X) (mod i 40) (inc X)) \# \.)))
       (partition 40)
       (map (partial apply str))
       (str/join \newline)))

(let [instructions (->> (get-puzzle-input 22 10)
                        str/split-lines
                        (map parse-instruction))]
  (println "Part 1:" (time (part-1 instructions)))
  (println "Part 2:")
  (println (part-2 instructions)))
