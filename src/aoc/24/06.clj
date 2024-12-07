(ns aoc.24.06
  (:require
   [aoc.lib.io :refer [get-puzzle-input]]
   [aoc.lib.matrix :refer [char-matrix mfind mshape v+]]
   [aoc.lib.seq :refer [find-first mapvals]]
   [clojure.string :as str]))

(def ^:private test-input
  "....#.....
.........#
..........
..#.......
.......#..
..........
.#..^.....
........#.
#.........
......#...")

; For our impementation, a "position" is a vector of a point in the matrix and a direction, e.g. `[[0 1] :n]`.

(defn- parse-input [input]
  (let [grid (char-matrix (str/split-lines input))
        obstrs (mfind grid #(= \# %))
        pos (first (mfind grid #(= \^ %)))
        dir :n]
    {:size (mshape grid),
     :obstructions {:all (set obstrs)
                    :hit-from {}
                    :by-row
                    (->> (reduce (fn [acc [i j]] (update acc i #(conj % j))) {} obstrs)
                         (mapvals #(vec (sort %)))),
                    :by-col
                    (->> (reduce (fn [acc [i j]] (update acc j #(conj % i))) {} obstrs)
                         (mapvals #(vec (sort %))))},
     :position [pos dir],
     :path []}))

(defn- turn-right [dir]
  (case dir :n :e, :e :s, :s :w, :w :n,
        (throw (ex-info "invalid direction" {:direction dir}))))

(def ^:private dirs {:n [-1 0], :e [0 1], :s [1 0], :w [0 -1]})
(defn- step [dir pos] (v+ pos (or (dirs dir) (throw (ex-info "invalid direction" {:direction dir})))))

(defn- ->dir [[i1 j1, :as p1] [i2 j2, :as p2]]
  (assert (not= p1 p2) "equal points are not supported")
  (assert (or (= i1 i2) (= j1 j2)) "only vertical or horizontal directions are supported")
  (cond (< i1 i2) :s, (> i1 i2) :n, (< j1 j2) :e, (> j1 j2) :w))

(defn- next-obstruction [{:keys [position obstructions]}]
  (let [[[row col] dir] position
        col? (#{:s :n} dir)
        rev? (#{:n :w} dir)
        x (if col? row col)
        x's (get-in obstructions [(if col? :by-col :by-row) (if col? col row)])
        x's (when x's (cond-> x's rev? rseq))
        cmp (if rev? > <)]
    (when-let [x' (find-first (fn [x'] (cmp x x')) x's)]
      (if col? [x' col] [row x']))))

(defn- ->line
  "Generate a seq of integer points on a straight line from start to end (exclusive).
  Does not work for diagonal lines."
  [[i1 j1, :as start] [i2 j2, :as end]]
  (cond
    (= i1 i2) (for [j' (range j1 j2 (if (< j1 j2) 1 -1))] [i1 j'])
    (= j1 j2) (for [i' (range i1 i2 (if (< i1 i2) 1 -1))] [i' j1])
    :else (throw (ex-info "diagonal lines are not supported" {:start start, :end end}))))

(defn- advance
  "Make one decision and execute it: Determine we're done because we're at the edge of the map,
  turn right because we're facing an obstacle, or just go as far as we can."
  [{:keys [obstructions position size], :as state}]
  (let [is-obstruction? (:all obstructions)
        off-grid? (let [[h w] size] (fn [[i j]] (or (< i 0) (< j 0) (>= i h) (>= j w))))
        [[i j, :as pos] dir] position
        next-step (step dir pos)]

    (cond
      ; We're facing the edge of the map. Report that we're finished.
      (off-grid? next-step)
      (assoc state :finished? true)

      ; We hit an obstruction. Gotta turn right.
      (is-obstruction? next-step)
      (-> state
          (assoc-in [:position 1] (turn-right dir))
          (update-in [:obstructions :hit-from next-step] (fn [dirs] (conj (or dirs #{}) dir))))

      ; No obstruction directly in front of us.
      ; Go as far as we can until we hit an obstruction or the end of the map.
      :else
      (let [dest (or (next-obstruction state)
                     (let [[h w] size] (case dir :n [-1 j], :e [i w], :s [h j], :w [i -1])))
            steps (rest (->line pos dest))]
        (-> state
            (update :path #(reduce conj % steps))
            (update :position #(assoc % 0 (last steps))))))))

(defn- simulate-patrol [state]
  (loop [state state] (if (:finished? state) state (recur (advance state)))))

(defn part-1 [state]
  (-> state simulate-patrol :path set count))

(defn- ends-in-loop? [{:keys [obstructions position size], :as state}]
  (let [is-obstruction? (:all obstructions),
        off-grid? (let [[h w] size] (fn [[i j]] (or (< i 0) (< j 0) (>= i h) (>= j w)))),
        [[i j, :as pos] dir] position,
        next-step (step dir pos)]

    (cond
      ; We're facing the edge of the map. There was no loop.
      (off-grid? next-step)
      false

      ; We hit an obstruction. Did we hit this one before from this direction?
      ; If so, there was a loop. Otherwise turn right and move on.
      (is-obstruction? next-step)
      (if (contains? (get-in obstructions [:hit-from next-step]) dir)
        true
        (ends-in-loop?
         (-> state
             (assoc-in [:position 1] (turn-right dir))
             (update-in [:obstructions :hit-from next-step] (fn [dirs] (conj (or dirs #{}) dir))))))

      ; No obstruction directly in front of us.
      ; Go as far as we can until we hit an obstruction or the end of the map.
      :else
      (let [dest (or (next-obstruction state)
                     (let [[h w] size] (case dir :n [-1 j], :e [i w], :s [h j], :w [i -1])))
            steps (rest (->line pos dest))]
        (ends-in-loop? (update state :position #(assoc % 0 (last steps))))))))

(defn- add-obstruction [state [i j, :as obstr]]
  (-> state
      (update-in [:obstructions :all] #(conj % obstr))
      (update-in [:obstructions :by-row i] #(vec (sort (conj % j))))
      (update-in [:obstructions :by-col j] #(vec (sort (conj % i))))))

(defn part-2 [state]
  (let [state' (simulate-patrol state)
        steps (partition 2 1 (:path state'))
        state' (-> state' (dissoc :path) (assoc-in [:obstructions :hit-from] {}))]
    (->> (for [[p1 p2] steps,
               :let [dir (->dir p1 p2),
                     state'' (-> state'
                                 (add-obstruction p2)
                                 (assoc :position [p1 dir]))],
               :when (ends-in-loop? state'')]
           p2)
         (distinct)
         (count))))

(let [state (parse-input #_test-input (get-puzzle-input 24 6))]
  (println "Part 1:" (time (part-1 state)))
  (println "Part 2:" (time (part-2 state))))

