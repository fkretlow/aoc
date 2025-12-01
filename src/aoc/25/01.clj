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

(defn- parse [input]
  (->> (str/split-lines input)
       (map #(-> [(case (first %) \L - \R +) (parse-long (subs % 1))]))))

(defn part-1 [ops]
  (->> (reductions (fn [x [op y]] (mod (op x y) 100)) 50 ops)
       (filter zero?)
       count))

(def ^:private test? true)

(let [input (if test? test-input (get-puzzle-input 25 1))
      ops (parse input)]
  (part-1 ops))

