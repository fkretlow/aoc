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
