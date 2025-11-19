(ns aoc.lib.math
  (:require [clojure.math :refer [ceil]]))


(defn lcm
  [& xs]
  (reduce #(.divide (.multiply %1 %2) (.gcd %1 %2)) (map biginteger xs)))


(defn next-greater-int
  "Return the closest integer strictly greater than `x`."
  [x]
  (let [n (int (ceil x))] (if (= (int x) n) (inc n) n)))


(defn shoelace
  "https://en.wikipedia.org/wiki/Shoelace_formula"
  [path]
  (abs (reduce (fn [acc [[i1 j1] [i2 j2]]]
                 (+ acc (/ (* (+ j1 j2) (- i1 i2)) 2.0)))
         0
         (partition 2 1 path))))


(defn ->line-parameters
  "Given two points on a line, return m and c in the forn `[m c]`
  such that y = mx + c is true for both points."
  [[p1 p2]]
  (let [[[x1 y1] [x2 y2]] (sort-by first [p1 p2]),
        m (/ (- y2 y1) (- x2 x1)),
        c (- y1 (* m x1))]
    [m c]))


(defn intersection-of-lines
  "Given the parameters of two lines y = mx + c in the form
  `[m1 c1] [m2 c2]`, return the intersection point of the two lines,
  if it exists. Returns `nil` if m1 = m2, regardless of whether
  the lines are indentical."
  [[m1 c1] [m2 c2]]
  (when-not (= m1 m2)
    (let [x (/ (- c2 c1) (- m1 m2))]
      [x (+ (* m1 x) c1)])))

