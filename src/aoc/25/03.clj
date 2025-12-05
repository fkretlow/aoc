(ns aoc.25.03
  (:require
   [aoc.lib.io :refer [get-puzzle-input]]
   [aoc.lib.range :as r]
   [aoc.lib.seq :refer [collect-indices]]
   [clojure.string :as str]))

(def ^:private test-input (str/trim "
987654321111111
811111111111119
234234234234278
818181911112111
"))

(defn- parse [input]
  (->> (str/split-lines input)
       (map (partial mapv #(- (int %) 48)))))

(defn max-joltage
  "Given a sequence of one-digit numbers `xs`, return the maximum number you can construct
  by selecting and concatenating `n` numbers."
  [n xs]
  {:pre [(<= n (count xs))]}
  (let [; the length of `xs`
        l (count xs)
        ; a vector of pairs of the form `[x is]` where `x` is an element of `xs`
        ; and `is` is a vector of indices of `x` in `xs` in ascending order,
        ; the outer vector sorted by `x` in descending order, e.g., if `xs` is
        ; `[1 3 1 2]`, `indices` will be `[[3 [1], 2 [3], [1 [0 2]]]]`
        indices (->> (collect-indices xs) seq (sort-by first) reverse)]
    (loop [; how many elements we need to choose
           n n
           ; the start (inclusive) of the subvector to consider for this step
           start 0
           ; the chosen elements
           xs_max []]
      (if (zero? n)
        ; we're done, sum up the joltage
        (reduce (fn [acc cur] (+ (* 10 acc) cur)) 0 xs_max)
        ; assuming we need to choose n elements in total, select the greatest one that
        ; still leaves n-1 numbers to choose from after it
        (let [; the end (exclusive) of the subvector to consider for this step
              end (- l (dec n))
              ; the largest element and its first index in the subvector
              [x_max i] (first (for [[x is] indices,
                                     :let [i (first (filter #(r/contains? [start end] %) is))]
                                     :when i]
                                 [x i]))]
          (recur (dec n) (inc i) (conj xs_max x_max)))))))

(defn part-1 [jss]
  (reduce + (map #(max-joltage 2 %) jss)))

(defn part-2 [jss]
  (reduce + (map #(max-joltage 12 %) jss)))

(def ^:private test? false)

(let [input (if test? test-input (get-puzzle-input 25 3))
      jss (parse input)]
  (println "Part 1" (time (part-1 jss)))
  (println "Part 2" (time (part-2 jss))))

