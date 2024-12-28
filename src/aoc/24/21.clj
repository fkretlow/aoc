(ns aoc.24.21
  (:require
   [aoc.lib.io :refer [get-puzzle-input]]
   [clojure.string :as str]))

(def ^:private keypads
  {:num {:7 [0 0], :8 [0 1], :9 [0 2],
         :4 [1 0], :5 [1 1], :6 [1 2],
         :1 [2 0], :2 [2 1], :3 [2 2],
         :gap [3 0], :0 [3 1], :A [3 2]}
   :dir {:gap [0 0], :n [0 1], :A [0 2],
         :w [1 0], :s [1 1], :e [1 2]}})

(defn- ->pos [ktype btn]
  (assert (contains? keypads ktype) (str "funny keypad type: " ktype))
  (assert (contains? (keypads ktype) btn) (str "unknown button: " ktype " " btn))
  (get-in keypads [ktype btn]))

(def ^:private ->path
  (memoize
   (fn [ktype [i j] [i' j']]
     ; Move horizontally or vertically first depending on which way we're sure
     ; to avoid the gap.
     (into [] (if (= i (-> (->pos ktype :gap) (nth 0)))
                (concat (for [i'' (range i i' (if (< i i') 1 -1))] [i'' j])
                        (for [j'' (range j j' (if (< j j') 1 -1))] [i' j''])
                        [[i' j']])
                (concat (for [j'' (range j j' (if (< j j') 1 -1))] [i j''])
                        (for [i'' (range i i' (if (< i i') 1 -1))] [i'' j'])
                        [[i' j']]))))))

(defn- step->dir [[[i j] [i' j']]]
  (assert (or (and (= i i') (= 1 (abs (- j' j))))
              (and (= j j') (= 1 (abs (- i' i)))))
          "can only move one step vertically or horizontally")
  (if (= i i') (if (< j j') :e :w) (if (< i i') :s :n)))

(defn- -push-one
  "Assuming the robot arm is currently pointing at the button labelled `cur-btn` on a
  keypad of the type `ktype`, calculate a minimal button sequence for the directional
  keypad controlling the robot that would make it push the button labelled `btn`."
  [ktype cur-btn btn]
  (let [p (->pos ktype cur-btn),
        p' (->pos ktype btn),
        path (->path ktype p p')]
    (conj (->> (partition 2 1 path) (mapv step->dir)) :A)))

(defn- -push-many
  "Assuming the robot arm is currently pointing at the button labelled `cur-btn` on a
  keypad of the type `ktype`, calculate a minimal button sequence for the directional
  keypad controlling the robot that would make it push all the buttons `btns` in the
  given order."
  [ktype cur-btn btns]
  (loop [cur-btn cur-btn,
         [btn & more-btns] btns,
         control-sequence []]
    (if-not btn
      control-sequence
      (recur btn more-btns (apply conj control-sequence (-push-one ktype cur-btn btn))))))

(defn- code->control-sequence
  "Assuming all robots are currently pointing at the button labelled A on their respective
  keypads, calculate a control sequence for you to type on your end so the final robot will
  input the desired `btns` on the numeric keypad."
  [code]
  (->> (map (comp keyword str) (seq code))
       (-push-many :num :A)
       (-push-many :dir :A)
       (-push-many :dir :A)))

(defn code->complexity
  [code]
  (let [numeric-part (parse-long (re-find #"\d+" code))
        control-sequence (code->control-sequence code)]
    (* (count control-sequence) numeric-part)))

(defn part-1 [codes]
  (apply + (map code->complexity codes)))

(def ^:private test-input "029A\n980A\n179A\n456A\n379A")

(let [codes (str/split-lines test-input #_(get-puzzle-input 24 21))]
  (println "Part 1:" (time (part-1 codes))))

