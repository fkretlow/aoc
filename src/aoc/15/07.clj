(ns aoc.15.07
  (:require [aoc.lib.io :refer [get-puzzle-input]]
            [clojure.string :as str]))

(def ^:private mask 16rFFFF)

(defn- try-coerce-to-long [s] (if (re-matches #"^\d+$" s) (parse-long s) s))

(defn- parse-node
  [line]
  (let [[term k] (str/split line #" -> ")
        [a b c] (map try-coerce-to-long (str/split term #"\s+"))]
    [k
     (cond (= "NOT" a) [:not b]
           (= "AND" b) [:and a c]
           (= "OR" b) [:or a c]
           (= "LSHIFT" b) [:lshift c a]
           (= "RSHIFT" b) [:rshift c a]
           (and (nil? b) (nil? c)) [:const a]
           :else (throw (ex-info (str "cannot parse line: " line)
                                 {:line line})))]))

(defn- parse-graph [spec] (into {} (map parse-node (str/split-lines spec))))

(defn solve
  [G]
  (let [cache (atom {})
        evaluate
          (fn -evaluate [G x]
            (when-not (contains? @cache x)
              (let [res (bit-and
                          mask
                          (if (int? x)
                            x
                            (let [[cmd & args] (get G x)]
                              (case cmd
                                :const (-evaluate G (first args))
                                :not (bit-not (-evaluate G (first args)))
                                :and (apply bit-and (map #(-evaluate G %) args))
                                :or (apply bit-or (map #(-evaluate G %) args))
                                :lshift (let [[n k] args]
                                          (bit-shift-left (-evaluate G k) n))
                                :rshift (let [[n k] args]
                                          (bit-shift-right (-evaluate G k)
                                                           n))))))]
                (swap! cache #(assoc % x res))))
            (get @cache x))]
    (evaluate G "a")))


(let [G (parse-graph (get-puzzle-input 15 7))]
  (println "Part 1:" (time (solve G))))
