(ns aoc.15.05
  (:require
   [aoc.lib.io :refer [get-puzzle-input]]
   [clojure.string :as str]))

(defn- nice? [s]
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

(defn- identical-pairs-reducer [{:keys [success? pairs prev], :as state} [i c]]
  (if success?
    state
    (let [state' (assoc state :prev c)]
      (if (= 0 i)
        state'
        (let [pair (str prev c), i-pair (dec i)]
          (if (some #(<= 2 (- i-pair %)) (get pairs pair))
            {:success? true}
            (update-in state' [:pairs pair] #(conj % i-pair))))))))

(defn- separated-twins-reducer [{:keys [success? pprev prev], :as state} [_ c]]
  (cond
    success? state
    (= pprev c) {:success? true}
    :else {:pprev prev, :prev c}))

(defn- nicer? [s]
  (->> (map vector (range) s)
       (reduce (fn [[identical-pairs-state separated-twins-state] [i c]]
                 [(identical-pairs-reducer identical-pairs-state [i c])
                  (separated-twins-reducer separated-twins-state [i c])])
               nil)
       (every? :success?)))

(defn part-2 [words] (count (filter nicer? words)))

(let [words (str/split-lines (get-puzzle-input 15 5))]
  (println "Part 1:" (time (part-1 words)))
  (println "Part 2:" (time (part-2 words))))
