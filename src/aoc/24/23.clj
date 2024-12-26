(ns aoc.24.23
  (:require
   [aoc.lib.graph :refer [find-max-cliques find-triangles]]
   [aoc.lib.io :refer [get-puzzle-input]]
   [clojure.string :as str]))

(defn parse [input]
  (->> (str/split-lines input)
       (map #(str/split % #"-"))
       (reduce (fn [G [v u]]
                 (-> G
                     (update v #(conj (or % #{}) u))
                     (update u #(conj (or % #{}) v))))
               {})))

(defn part-1 [G]
  (->> (find-triangles G)
       (filter (fn [vs] (some #(str/starts-with? % "t") vs)))
       (count)))

(defn part-2 [G]
  (->> (find-max-cliques G)
       (sort-by count >)
       (first)
       (sort)
       (str/join ",")))

(let [G (parse (get-puzzle-input 24 23))]
  (println "Part 1:" (time (part-1 G)))
  (println "Part 2:" (time (part-2 G))))
