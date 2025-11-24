(ns aoc.22.16
  (:require
   [clojure+.hashp :as hashp]
   [clojure.string :as str]))

(hashp/install!)

(def ^:private test-input (str/trim "
 Valve AA has flow rate=0; tunnels lead to valves DD, II, BB
 Valve BB has flow rate=13; tunnels lead to valves CC, AA
 Valve CC has flow rate=2; tunnels lead to valves DD, BB
 Valve DD has flow rate=20; tunnels lead to valves CC, AA, EE
 Valve EE has flow rate=3; tunnels lead to valves FF, DD
 Valve FF has flow rate=0; tunnels lead to valves EE, GG
 Valve GG has flow rate=0; tunnels lead to valves FF, HH
 Valve HH has flow rate=22; tunnel leads to valve GG
 Valve II has flow rate=0; tunnels lead to valves AA, JJ
 Valve JJ has flow rate=21; tunnel leads to valve II
 "))

(defn- parse-valve
  "Parses a single valve into a map containg
  - `:label`
  - `:flow-rate`
  - `:adjacent-valves` as a seq of valve labels"
  [s]
  (let [flow-rate (parse-long (re-find #"\d+" s)),
        [label & adjacent-valves] (re-seq #"[A-Z]{2}" s)]
    {:label label, :flow-rate flow-rate, :adjacent-valves adjacent-valves}))

(defn- parse
  "Returns the graph as a map containing
  - `:vertices` as a map from labels to flow-rates
  - `:edges` as a map from labels to connected labels (adjacency lists)"
  [input]
  (->> (str/split-lines input)
       (reduce (fn [{:keys [vertices edges], :as _graph} valve-spec]
                 (let [{:keys [label flow-rate adjacent-valves]} (parse-valve valve-spec)
                       vertices' (assoc vertices label flow-rate),
                       edges' (assoc edges label (set adjacent-valves))]
                   {:vertices vertices' :edges edges'}))
               nil)))

(defn- find-steps [{:keys [graph position pressure]}]
  (-> (when-not (or (contains? (pressure :open-valves) position)
                    (zero? (get-in graph [:vertices position])))
        [[:open position]])
      (concat (for [valve (get-in graph [:edges position])] [:move valve]))))

(defmulti apply-step (fn [_state [op _arg]] op))

(defmethod apply-step :open
  [{:keys [graph pressure], :as state} [_op valve]]
  {:pre [(not (contains? (:open-valves pressure) valve))]}
  (-> state
      (update-in [:pressure :open-valves] #(conj % valve))
      (update-in [:pressure :total] #(+ % (pressure :per-minute)))
      (update-in [:pressure :per-minute] #(+ % (get-in graph [:vertices valve])))
      (update :minute inc)))

(defmethod apply-step :move
  [{:keys [graph position pressure], :as state}
   [_op valve]]
  {:pre [(contains? (get-in graph [:edges position]) valve)]}
  (-> state
      (assoc :position valve)
      (update-in [:pressure :total] #(+ % (pressure :per-minute)))
      (update :minute inc)))

(defn dfs [{:keys [minute pressure], :as state}]
  (if (>= minute 30)
    (-> pressure :total)
    (->> (find-steps state)
         (map #(apply-step state %))
         (map dfs)
         (apply max))))

(let [graph (parse test-input)
      state {:graph graph,
             :position "AA",
             :minute 0,
             :pressure {:total 0, :per-minute 0, :open-valves #{}}}]
  state)

