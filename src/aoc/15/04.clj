(ns aoc.15.04
  (:import
   [java.security MessageDigest]
   [java.util HexFormat]))

(def ^:private md5
  (let [md (MessageDigest/getInstance "MD5"),
        fmt (HexFormat/of)]
    (fn [s] (apply str (mapcat #(.toHexDigits fmt %) (.digest md (.getBytes s)))))))

(defn- count-leading [c s] (count (take-while #(= c %) s)))

(defn part-1 [input]
  (->> (range)
       (filter (fn [n]
                 (when (= 0 (mod n 10000)) (println n))
                 (->> (str input n)
                      (md5)
                      (count-leading \0)
                      (<= 5))))
       (first)))

(let [input "iwrupvqb"]
  (println "Part 1:" (time (part-1 input))))
