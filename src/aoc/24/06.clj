(ns aoc.24.06
  (:require
   [aoc.lib.io :refer [get-puzzle-input]]
   [aoc.lib.matrix :refer [char-matrix mfind mshape]]
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

; For our impementation, a "position" is a vector of a point in the matrix and a direction, e.g. `[[0 1] :north]`.

(defn- parse-input [input]
  (let [lab-map (char-matrix (str/split-lines input))
        obstrs (mfind lab-map #(= \# %))
        pos (first (mfind lab-map #(= \^ %)))
        dir :north]
    {:size (mshape lab-map),
     :obstructions {:all (set obstrs)
                    :by-row
                    (->> (reduce (fn [acc [i j]] (update acc i #(conj % j))) {} obstrs)
                         (mapvals #(vec (sort %)))),
                    :by-column
                    (->> (reduce (fn [acc [i j]] (update acc j #(conj % i))) {} obstrs)
                         (mapvals #(vec (sort %))))},
     :position [pos dir],
     :path []}))

(defn- turn-right [dir]
  (case dir :north :east, :east :south, :south :west, :west :north,
        (throw (ex-info "invalid direction" {:direction dir}))))

(defn- north [[i j]] [(dec i) j])
(defn- east [[i j]] [i (inc j)])
(defn- south [[i j]] [(inc i) j])
(defn- west [[i j]] [i (dec j)])
(defn- step [dir pos] ((case dir :north north :east east :south south :west west) pos))

(defn next-obstruction [{:keys [position obstructions]}]
  (let [[[row col] dir] position
        col? (#{:south :north} dir)
        rev? (#{:north :west} dir)
        x (if col? row col)
        x's (get-in obstructions [(if col? :by-column :by-row) (if col? col row)])
        x's (if rev? (rseq x's) x's)
        cmp (if rev? > <)]
    (when-let [x' (find-first (fn [x'] (cmp x x')) x's)]
      (if col? [x' col] [row x']))))

; (assert (= (next-obstruction (assoc (parse-input ">...#") :position [[0 0] :east])) [0 4]))
; (assert (= (next-obstruction (assoc (parse-input ".#..<") :position [[0 4] :west])) [0 1]))
; (assert (= (next-obstruction (assoc (parse-input "V\n.\n.\n#") :position [[0 0] :south])) [3 0]))
; (assert (= (next-obstruction (assoc (parse-input "#\n.\n.\n^") :position [[3 0] :north])) [0 0]))
; (assert (nil? (next-obstruction (assoc (parse-input ">...") :position [[0 0] :east]))))

(defn ->line
  "Generate a seq of integer points on a straight line from start to end (exclusive).
  Does not work for diagonal lines."
  [[i1 j1, :as start] [i2 j2, :as end]]
  (cond
    (= i1 i2) (for [j' (range j1 j2 (if (< j1 j2) 1 -1))] [i1 j'])
    (= j1 j2) (for [i' (range i1 i2 (if (< i1 i2) 1 -1))] [i' j1])
    :else (throw (ex-info "diagonal lines are not supported" {:start start, :end end}))))

(defn advance
  "Make one decision and execute it: Determine we're done because we're at the edge of the map,
  turn right because we're facing an obstacle, or just go as far as we can."
  [{:keys [obstructions position size], :as state}]
  (let [is-obstr? (:all obstructions)
        off-grid? (let [[h w] size] (fn [[i j]] (or (< i 0) (< j 0) (>= i h) (>= j w))))
        [[i j, :as pos] dir] position]

    (cond
      ; We're facing the edge of the map. Report that we're finished.
      (off-grid? (step dir pos))
      (assoc state :finished? true)

      ; We hit an obstruction. Gotta turn right.
      (is-obstr? (step dir pos))
      (assoc-in state [:position 1] (turn-right dir))

      ; No obstruction directly in front of us.
      ; Go as far as we can until we hit an obstruction or the end of the map.
      :else
      (let [dest (or (next-obstruction state)
                     (let [[imax jmax] size] (case dir :north [-1 j], :east [i jmax], :south [imax j], :west [i -1])))
            steps (->line pos dest)]
        (-> state
            (update :path #(reduce conj % steps))
            (update :position #(assoc % 0 (last steps))))))))

(defn simulate-patrol [state]
  (loop [state state] (if (:finished? state) state (recur (advance state)))))

(defn part-1 [state]
  (-> state simulate-patrol :path set count))

(let [state (parse-input (get-puzzle-input 24 6))]
  (println "Part 1:" (time (part-1 state))))
