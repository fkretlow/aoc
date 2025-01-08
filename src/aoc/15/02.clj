(ns aoc.15.02
  (:require
   [aoc.lib.io :refer [get-puzzle-input]]
   [aoc.lib.parsing :refer [parse-longs]]
   [clojure.string :as str]))

(defn- parse-parcels [input] (map parse-longs (str/split-lines input)))
(defn- ->areas [[l w h]] [(* l w) (* w h) (* l h)])
(defn- ->surface [areas] (reduce + (map #(* 2 %) areas)))

(defn part-1 [parcels]
  (->> (map ->areas parcels)
       (mapcat (juxt ->surface #(apply min %)))
       (reduce +)))

(defn part-2 [parcels]
  (->> (map sort parcels)
       (mapcat (juxt #(->> % (take 2) (apply +) (* 2))
                     #(apply * %)))
       (reduce +)))

(let [parcels (parse-parcels (get-puzzle-input 15 2))]
  (println "Part 1:" (time (part-1 parcels)))
  (println "Part 2:" (time (part-2 parcels))))
