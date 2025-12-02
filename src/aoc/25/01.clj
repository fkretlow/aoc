(ns aoc.25.01
  (:require
   [aoc.lib.io :refer [get-puzzle-input]]
   [clojure.string :as str]))

(def ^:private test-input (str/trim "
L68
L30
R48
L5
R60
L55
L1
L99
R14
L82"))

(defn parse
  "Given an input string consisting of lines matching the pattern `(L|R)\\d+`,
  parse it into a seq of numbers, e.g. `L68 -> -68`, `R48 -> 48`."
  [input]
  (->> (str/split-lines input)
       (map #(* (case (first %) \L -1 \R 1) (parse-long (subs % 1))))))

(defn part-1 [dxs]
  (->> (reductions (fn [x dx] (mod (+ x dx) 100)) 50 dxs)
       (filter zero?)
       count))

(defn- clicks-at-0
  "Assuming the dial is pointing at `x` and we rotate it by `dx`, calculate
  how many times the dial will be pointing at 0 along the way, including at
  the end of the rotation."
  [x dx]
  (cond
    (zero? dx) 0

    (pos? dx)
    (loop [c0s 0, x x, dx dx]
      (let [d0 (- 100 x)]
        (cond
          (< dx d0) c0s,
          (= dx d0) (inc c0s),
          (> dx d0) (recur (inc c0s) 0 (- dx d0)))))

    (neg? dx)
    (loop [c0s 0, x (if (zero? x) 100 x), dx dx]
      (let [d0 (- x)]
        (cond
          (> dx d0) c0s,
          (= dx d0) (inc c0s)
          (< dx d0) (recur (inc c0s) 100 (- dx d0)))))))

(defn part-2 [dxs]
  (->> (reduce
        (fn [[x c0s] dx] [(mod (+ x dx) 100) (+ c0s (clicks-at-0 x dx))])
        [50 0]
        dxs)
       second))

(def ^:private test? false)

(let [input (if test? test-input (get-puzzle-input 25 1))
      ops (parse input)]
  (println "Part 1: " (time (part-1 ops)))
  (println "Part 2: " (time (part-2 ops))))


