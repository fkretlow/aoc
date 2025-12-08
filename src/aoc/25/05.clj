(ns aoc.25.05
  (:require
   [aoc.lib.io :refer [get-puzzle-input]]
   [aoc.lib.parsing :refer [parse-longs]]
   [aoc.lib.range-set :as rs]
   [clojure.string :as str]))

(def ^:private test-input (str/trim "
3-5
10-14
16-20
12-18

1
5
8
11
17
32"))

(defn- parse [input]
  (let [[range-str ids-str] (str/split input #"\n\n")
        fresh-id-ranges (->> range-str
                             str/split-lines
                             (map #(str/split % #"-"))
                             (map #(map parse-long %))
                             ; create a "range-set" for simple set-like operations
                             (map (fn [[a b]] [a (inc b)]))
                             (rs/merge-overlapping))
        ids (parse-longs ids-str)]
    [ids fresh-id-ranges]))

(defn part-1 [ids fresh-id-ranges]
  (count (filter #(rs/contains? fresh-id-ranges %) ids)))

(defn part-2 [fresh-id-ranges]
  (rs/count fresh-id-ranges))

(def ^:private test? false)

(let [input (if test? test-input (get-puzzle-input 25 5))
      [ids fresh-id-ranges] (parse input)]
  (println "Part 1:" (time (part-1 ids fresh-id-ranges)))
  (println "Part 2:" (time (part-2 fresh-id-ranges))))

