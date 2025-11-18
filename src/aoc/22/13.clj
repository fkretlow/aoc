(ns aoc.22.13
  (:require
   [aoc.lib.io :refer [get-puzzle-input]]
   [clojure.edn :as edn]
   [clojure.string :as str]))

;; See https://adventofcode.com/2022/day/13

(def ^:private test-input (str/trim "
 [1,1,3,1,1]
 [1,1,5,1,1]

 [[1],[2,3,4]]
 [[1],4]

 [9]
 [[8,7,6]]

 [[4,4],4,4]
 [[4,4],4,4,4]

 [7,7,7,7]
 [7,7,7]

 []
 [3]

 [[[]]]
 [[]]

 [1,[2,[3,[4,[5,6,7]]]],8,9]
 [1,[2,[3,[4,[5,6,0]]]],8,9]"))

(defmulti cmp
  "Comparator for packet trees. Dispatches on the types of its arguments."
  {:private true}
  (fn [& args]
    (mapv (fn [x] (cond
                    (seqable? x) :seq
                    (number? x) :num
                    :else (throw (ex-info (str "unexpected element " x) {:element x}))))
          args)))
(defmethod cmp [:num :num] [x y] (compare x y))
(defmethod cmp [:num :seq] [x ys] (cmp [x] ys))
(defmethod cmp [:seq :num] [xs y] (cmp xs [y]))
(defmethod cmp [:seq :seq] [xs ys]
  (cond
    (and (empty? xs) (empty? ys)) 0,
    (empty? xs) -1,
    (empty? ys) 1,
    :else (let [res (cmp (first xs) (first ys))]
            (if (not= 0 res)
              res
              (recur (rest xs) (rest ys))))))

(defn- p<
  "Less-than (<) for packet trees."
  [p1 p2]
  (< (cmp p1 p2) 0))

(defn part-1 [packets]
  (->> (partition 2 packets)
       (map (fn [[p1 p2]] (not (p< p2 p1))))
       (map-indexed vector)
       (filter second)
       (map first)
       (map inc)
       (apply +)))

(def ^:private divider-packets #{[[2]] [[6]]})

(defn part-2 [packets]
  (->> (concat packets divider-packets)
       (sort p<)
       (map-indexed vector)
       (filter (comp divider-packets second))
       (map first)
       (map inc)
       (apply *)))

(def ^:private test? false)

(let [input (if test? test-input (get-puzzle-input 22 13))
      packets (->> (str/split input #"\n+") (map edn/read-string))]
  (println "Part 1:" (time (part-1 packets)))
  (println "Part 2:" (time (part-2 packets))))
