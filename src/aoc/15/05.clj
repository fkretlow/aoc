(ns aoc.15.05 
  (:require
   [aoc.lib.io :refer [get-puzzle-input]]
   [clojure.string :as str]))

(defn nice? [s]
  (let [{:keys [vowels has-pair? has-forbidden?]}
        (reduce (fn [{:keys [prev], :as state} c]
                  (if (#{"ab" "cd" "pq" "xy"} (str prev c))
                    (reduced (assoc state :has-forbidden? true))
                    (cond-> (assoc state :prev c)
                      (#{\a \e \i \o \u} c) (update :vowels inc)
                      (= prev c) (assoc :has-pair? true))))
                {:vowels 0, :has-pair? false, :has-forbidden? false, :prev nil}
                s)]
    (and (not has-forbidden?) has-pair? (<= 3 vowels))))

(defn part-1 [words] (count (filter nice? words)))

(let [words (str/split-lines (get-puzzle-input 15 5))]
  (println "Part 1:" (time (part-1 words))))

(partition 2 1 "abcd")
