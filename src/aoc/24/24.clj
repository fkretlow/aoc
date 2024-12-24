(ns aoc.24.24
  (:require
   [aoc.lib.io :refer [get-puzzle-input]]
   [clojure.string :as str]))

(defn- parse [input]
  (let [[inputs-str combinators-str] (str/split input #"\n\n")]
    (into {}
          (concat
           (->> (str/split-lines inputs-str)
                (map #(str/split % #": "))
                (map (fn [[k v]] [k (parse-long v)])))
           (->> (str/split-lines combinators-str)
                (map #(str/split % #" "))
                (map (fn [[k1 op k2 _ k]] [k [(keyword (str/lower-case op)) k1 k2]])))))))

(defn part-1 [G]
  (let [eval (let [cache (atom {})]
               (fn -eval [k]
                 (when-not (contains? @cache k)
                   (swap! cache
                          #(assoc % k
                                  (let [n (get G k)]
                                    (if (int? n)
                                      n
                                      (let [[op k1 k2] n,
                                            v1 (-eval k1),
                                            v2 (-eval k2)]
                                        (case op
                                          :and (if (= 1 v1 v2) 1 0),
                                          :or (if (or (= 1 v1) (= 1 v2)) 1 0),
                                          :xor (if (not= v1 v2) 1 0))))))))
                 (get @cache k)))]
    (->> (keys G)
         (filter #(str/starts-with? % "z"))
         (sort)
         (map eval)
         (map-indexed (fn [e bit] (apply * bit (repeat e 2))))
         (apply +))))

(let [G (parse (get-puzzle-input 24 24))]
  (println "Part 1:" (time (part-1 G))))

