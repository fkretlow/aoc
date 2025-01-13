(ns aoc.15.06
  (:require
   [aoc.lib.io :refer [get-puzzle-input]]
   [aoc.lib.parsing :refer [parse-longs]]
   [clojure.string :as str]))

(defn- parse-mistranslated-command [input]
  (let [cmd (cond (str/starts-with? input "toggle") :toggle,
                  (str/starts-with? input "turn on") :turn-on,
                  (str/starts-with? input "turn off") :turn-off,
                  :else (throw (ex-info (str "funny command: " input) {:input input})))
        [x1 y1 x2 y2] (parse-longs input)]
    (fn [[[x y] b]]
      [[x y]
       (if (and (<= x1 x x2) (<= y1 y y2))
         (case cmd :toggle (not b), :turn-on true, :turn-off false)
         b)])))

(defn part-1 [commands]
  (let [f (apply comp (reverse commands))]
    (->> (for [x (range 1000), y (range 1000)] [[x y] false])
         (pmap f)
         (filter (comp true? second))
         (count))))

(defn- parse-correct-command [input]
  (let [cmd (cond (str/starts-with? input "toggle") :toggle,
                  (str/starts-with? input "turn on") :turn-on,
                  (str/starts-with? input "turn off") :turn-off,
                  :else (throw (ex-info (str "funny command: " input) {:input input})))
        [x1 y1 x2 y2] (parse-longs input)]
    (fn [[[x y] n]]
      [[x y]
       (if (and (<= x1 x x2) (<= y1 y y2))
         (case cmd :toggle (+ 2 n), :turn-on (inc n), :turn-off (max (dec n) 0))
         n)])))

(defn part-2 [commands]
  (let [f (apply comp (reverse commands))]
    (->> (for [x (range 1000), y (range 1000)] [[x y] 0])
         (pmap (comp second f))
         (reduce +))))

(let [commands (map parse-mistranslated-command (str/split-lines (get-puzzle-input 15 6)))]
  (println "Part 1:" (time (part-1 commands))))

(let [commands (map parse-correct-command (str/split-lines (get-puzzle-input 15 6)))]
  (println "Part 2:" (time (part-2 commands))))
