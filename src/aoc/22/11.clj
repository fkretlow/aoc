(ns aoc.22.11
  (:require
   [aoc.lib.io :refer [get-puzzle-input]]
   [aoc.lib.parsing :refer [parse-longs]]
   [clojure.math :refer [floor-div]]
   [clojure.string :as str]))

;; See https://adventofcode.com/2022/day/11

(def ^:private test-input (str/trim "
Monkey 0:
  Starting items: 79, 98
  Operation: new = old * 19
  Test: divisible by 23
    If true: throw to monkey 2
    If false: throw to monkey 3

Monkey 1:
  Starting items: 54, 65, 75, 74
  Operation: new = old + 6
  Test: divisible by 19
    If true: throw to monkey 2
    If false: throw to monkey 0

Monkey 2:
  Starting items: 79, 60, 97
  Operation: new = old * old
  Test: divisible by 13
    If true: throw to monkey 1
    If false: throw to monkey 3

Monkey 3:
  Starting items: 74
  Operation: new = old + 3
  Test: divisible by 17
    If true: throw to monkey 0
    If false: throw to monkey 1
"))

(defn- parse-monkey [monkey-str]
  (let [[_ starting-items-str operation-str test-str if-true-str if-false-str] (str/split-lines monkey-str)]
    {:state {:items (parse-longs starting-items-str),
             :inspection-count 0},
     :spec {:op (-> (second (str/split operation-str #" = "))
                    (str/split #"\s+")
                    (as-> parts (map
                                 (fn [part] (if (re-matches #"\d+" part)
                                              (parse-long part)
                                              (keyword part)))
                                 parts))
                    ((fn [[arg1 op arg2]] [op arg1 arg2]))),
            :throw (mapv (comp first parse-longs) [test-str if-true-str if-false-str])}}))

(defn perform-operation
  "Apply the operation defined by `op-spec` to `x`.
  `op-spec` must be of the form `[<operator> & <args>]`, where operator is one of `:+`, `:*`,
  and args is a seq of arguments, each of them either a number or `:old`. The specified function
  will be applied to the arguments with `:old` replaced by the value of `x`."
  [[op & args, :as _op-spec] x]
  (apply (case op :+ + :* *) (map #(if (= :old %) x %) args)))

(defn calculate-throw-target
  "Given a monkey throw spec and an item worry level, calculate where to throw the item."
  [[divisor then else, :as _throw-spec] item-worry-level]
  (if (zero? (mod item-worry-level divisor)) then else))

(defn inspect-item
  "Given a monkey spec and an item worry level, simulate the monkey inspecting the
  item. Return a pair of the form `[throw-target new-item-worry-level]`."
  [{op-spec :op throw-spec :throw, :as _monkey-spec} item-worry-level]
  (let [new-item-worry-level (as-> item-worry-level x (perform-operation op-spec x) (floor-div x 3))
        throw-target (calculate-throw-target throw-spec new-item-worry-level)]
    [throw-target new-item-worry-level]))

(defn turn
  "Given a monkey's spec and state, simulate the monkey taking a single turn
  and inspecting/throwing all its items. Return pair of the form
  `[new-monkey-state {0 items-for-monkey-0, 1 items-for-monkey-1, ...}]`"
  [monkey-spec monkey-state]
  (let [items (:items monkey-state)
        monkey-state' (-> monkey-state
                          (dissoc :items)
                          (update :inspection-count #(+ % (count items))))
        throw-map (->> items
                       (map (partial inspect-item monkey-spec))
                       (map (fn [[throw-target item]] {throw-target [item]}))
                       (apply merge-with into))]
    [monkey-state' throw-map]))

(defn round
  "Given all the monkey specs and states at the start of a round,
  simulate all of the monkeys inpecting/throwing all their items
  one by one. Return the new monkey states."
  [monkey-specs monkey-states]
  (loop [monkey-states monkey-states
         i 0]
    (if (= (count monkey-states) i)
      monkey-states
      (let [spec (nth monkey-specs i)
            state (nth monkey-states i)
            [state' throw-map] (turn spec state)]
        (-> monkey-states
            (assoc i state')
            (as-> states
                  (reduce (fn [states [i items]] (update-in states [i :items] #(concat % items)))
                          states
                          throw-map))
            (recur (inc i)))))))

(defn part-1 [monkeys]
  (let [monkey-specs (mapv :spec monkeys)
        monkey-states (mapv :state monkeys)]
    (->> (-> (iterate (partial round monkey-specs) monkey-states) (nth 20))
         (map :inspection-count)
         (sort >)
         (take 2)
         (apply *))))

(def ^:private test? false)

(let [input (if test? test-input (get-puzzle-input 22 11))
      monkeys (map parse-monkey (str/split input #"\n\n"))]
  (println "Part 1:" (time (part-1 monkeys))))
