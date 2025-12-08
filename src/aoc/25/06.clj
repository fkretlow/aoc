(ns aoc.25.06
  (:require
   [aoc.lib.io :refer [get-puzzle-input]]
   [aoc.lib.matrix :refer [char-matrix transpose]]
   [aoc.lib.parsing :refer [parse-longs]]
   [aoc.lib.seq :refer [split]]
   [clojure.string :as str]))

(def ^:private test-input "123 328  51 64 \n 45 64  387 23 \n  6 98  215 314\n*   +   *   +")

(defn- parse-1 [input]
  (let [lines (str/split-lines input)
        ops (->> (str/split (last lines) #"\s+")
                 (map #(case % "*" * "+" +)))
        arglists (->> (drop-last lines)
                      ; parse lines of ints
                      (map parse-longs)
                      ; collect into columns
                      (apply map vector))]
    (map vector ops arglists)))

(defn part-1 [input]
  (->> (parse-1 input)
       (map (fn [[op args]] (apply op args)))
       (apply +)))

(defn- parse-2 [input]
  (let [lines (str/split-lines input)
        ops (->> (str/split (last lines) #"\s+")
                 (map #(case % "*" * "+" +)))
        arglists (->> (drop-last lines)
                      ; create a char matrix and transpose it
                      ; so we can parse numbers left-to-right as usual
                      char-matrix
                      transpose
                      ; transform rows of chars into strings, drop whitespace
                      (map #(str/trim (apply str %)))
                      ; parse numbers or insert `nil` as a group separator
                      ; on empty strings for the next step
                      (map #(if (str/blank? %) nil (parse-long %)))
                      ; split into groups
                      (split nil?))]
    (map vector ops arglists)))

(defn part-2 [input]
  (->> (parse-2 input)
       (map (fn [[op args]] (apply op args)))
       (apply +)))

(def ^:private test? false)

(let [input (if test? test-input (get-puzzle-input 25 6))]
  (println "Part 1:" (time (part-1 input)))
  (println "Part 2:" (time (part-2 input))))
