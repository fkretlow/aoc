(ns aoc.24.25
  (:require
   [aoc.lib.io :refer [get-puzzle-input]]
   [clojure.set :as set]
   [clojure.string :as str]))

(def ^:private width 5)
(def ^:private max-height 5)
(def ^:private base-row (apply str (repeat width \#)))
(def ^:private open-row (apply str (repeat width \.)))

(defn- parse-item [s]
  (let [s (str/trim s)
        kind (cond (and (str/starts-with? s base-row) (str/ends-with? s open-row)) :lock,
                   (and (str/starts-with? s open-row) (str/ends-with? s base-row)) :key,
                   :else (throw (ex-info (format "funny lock/key:\n%s" s) {:item s})))
        profile (->> (str/split-lines s)
                     (map str/trim)
                     (apply map vector)
                     (map (fn [chars] (count (filter #(= \# %) chars))))
                     (map + (repeat -1)))]
    [kind profile]))

(defn parse [input]
  (let [item-strs (str/split input #"\n\n")]
    (->> (map parse-item item-strs)
         (map-indexed cons)
         (reduce (fn [m [id kind profile]]
                   (assoc-in m [(case kind :lock :locks, :key :keys) id] profile))
                 {}))))

(defn analyze-profiles
  "Given a seq of pairs of the form `[id profile]`, collect a nested vector of sets where
  the set at index `[i, hmax], 0 <= hmax <= 5` contains the ids of all the profiles with
  `h <= hmax` at position `i`."
  [profiles-with-ids]
  (vec
   (for [i (range width)]
     (vec
      (for [hmax (range (inc max-height))]
        (set (for [[id profile] profiles-with-ids,
                   :when (<= (nth profile i) hmax)]
               id)))))))

(defn part-1 [{locks :locks, ks :keys}]
  (let [keys-by-hmax (analyze-profiles ks)]
    (->> (for [profile (vals locks)
               :let [max-key-heights (map - (repeat max-height) profile)]]
           (->> max-key-heights
                (map-indexed (fn [i hmax] (get-in keys-by-hmax [i hmax])))
                (apply set/intersection)
                count))
         (apply +))))

(let [items (parse (get-puzzle-input 24 25))]
  (println "Part 1:" (time (part-1 items))))
