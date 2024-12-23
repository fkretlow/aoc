(ns aoc.24.19
  (:require
   [aoc.lib.io :refer [get-puzzle-input]]
   [clojure.string :as str]))

(defn- parse [input]
  (let [[patterns-str designs-str] (str/split input #"\n\n")]
    {:patterns (str/split patterns-str #", "),
     :designs (str/split-lines designs-str)}))

(defn part-1 [{:keys [patterns designs]}]
  (let [possible?
        (let [cache (atom {})]
          (fn -possible? [design]
            (when-not (contains? @cache design)
              (swap! cache
                     #(assoc %
                             design
                             (or (empty? design)
                                 (some true? (for [pattern patterns,
                                                   :when (str/starts-with? design pattern)]
                                               (-possible? (subs design (count pattern)))))))))
            (get @cache design)))]
    (count (filter possible? designs))))

(defn part-2 [{:keys [patterns designs]}]
  (let [num-solutions
        (let [cache (atom {})]
          (fn -num-solutions [design]
            (when-not (contains? @cache design)
              (swap! cache
                     #(assoc %
                             design
                             (->> (for [pattern patterns]
                                    (cond
                                      (= pattern design) 1
                                      (str/starts-with? design pattern) (-num-solutions (subs design (count pattern)))
                                      :else 0))
                                  (apply +)))))
            (get @cache design)))]
    (apply + (map #(num-solutions %) designs))))

(let [problem (parse (get-puzzle-input 24 19))]
  (println "Part 1:" (time (part-1 problem)))
  (println "Part 2:" (time (part-2 problem))))
