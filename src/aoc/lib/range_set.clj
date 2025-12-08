(ns aoc.lib.range-set
  "Set operations on range sets, i.e. sets of integers expressed in
  terms of seqs of integer ranges of the form [start end) where the
  individual ranges don't overlap or touch."
  (:refer-clojure :exclude [contains? count]))


(defn merge-overlapping [range-set]
  (letfn [(-merge? [r1 r2] (let [[r1 r2] (sort [r1 r2])] (>= (r1 1) (r2 0))))
          (-merge [r1 r2] [(min (r1 0) (r2 0)) (max (r1 1) (r2 1))])]
    (sort
     (reduce
      (fn [acc r]
        (if (and (seq acc) (-merge? r (first acc)))
          (conj (rest acc) (-merge r (first acc)))
          (conj acc r)))
      nil
      (sort (map vec range-set))))))


(defn- exclusion-steps [[start end]] [[start dec] [end inc]])
(defn- inclusion-steps [[start end]] [[start inc] [end dec]])
(defn- steps->ranges [min-true start-true steps]
  (loop [rs (transient [])
         cur-true start-true
         start 0
         steps steps]
    (if (seq steps)
      (let [[i op] (first steps)
            cur-true' (op cur-true)]
        (recur (if (and (= min-true cur-true) (> min-true cur-true')) (conj! rs [start i]) rs)
               cur-true'
               (if (and (> min-true cur-true) (= min-true cur-true')) i start)
               (rest steps)))
      (filterv #(< (% 0) (% 1)) (persistent! rs)))))


(defn union [& range-sets]
  (->> (sort-by first (mapcat #(mapcat inclusion-steps %) range-sets))
       (steps->ranges 1 0)
       (merge-overlapping)))


(defn difference [& range-sets]
  (let [n (clojure.core/count range-sets)]
    (->> (sort-by first (concat (mapcat inclusion-steps (first range-sets))
                                (mapcat #(mapcat exclusion-steps %) (rest range-sets))))
         (steps->ranges n (dec n)))))


(defn intersection [& range-sets]
  (->> (sort-by first (mapcat #(mapcat inclusion-steps %) range-sets))
       (steps->ranges (clojure.core/count range-sets) 0)))


(defn count-elements [range-set]
  (apply + (map #(- (% 1) (% 0)) range-set)))


(defn intersect? [& range-sets]
  (loop [cur-true 0
         i 0
         steps (sort-by first (mapcat #(mapcat inclusion-steps %) range-sets))]
    (if (seq steps)
      (let [[j op] (first steps)
            cur-true' (op cur-true)]
        (if (and (< i j) (= (clojure.core/count range-sets) cur-true) (< cur-true' cur-true))
          true
          (recur cur-true' j (rest steps))))
      false)))


(defn contains? [range-set n]
  (boolean (some #(< (dec (% 0)) n (% 1)) range-set)))


(defn count
  "Count the elements in the range set. Assumes that ranges are not
  overlapping, i.e. you might need to call `merge-overlapping` first."
  [range-set]
  (apply + (map (fn [[start end]] (- end start)) range-set)))
