(ns aoc.lib.matrix)


(defn transpose
  "Transpose a two-dimensional matrix."
  [matrix] (apply (partial map vector) matrix))


(defn find-row-indices
  "Find the indices of all the rows of a two-dimensional matrix that satisfy pred."
  [pred matrix]
  (->> (map-indexed #(-> [%1 %2]) matrix)
       (filter #(pred (second %)))
       (map first)))


(defn find-col-indices
  "Find the indices of all the columns of a two-dimensional matrix that satisfy pred."
  [pred matrix]
  (->> (transpose matrix)
       (find-row-indices pred)))


(defn find-indices
  "Find the indices of all the elements of a two-dimensional matrix that satisfy pred."
  [pred matrix]
  (->> (for [row matrix] (filter some? (map-indexed (fn [j x] (when (pred x) j)) row)))
       (map-indexed (fn [i js] (when (seq js) (for [j js] [i j]))))
       (filter some?)
       (apply concat)))


(defn char-matrix [lines]
  (assert (apply = (map count lines)) "different line lengths")
  (into-array (map char-array lines)))


(defn mget [m [i j]] (aget m i j))
(defn mset [m [i j] x] (doto m (aset i j x)))
(defn mshape [m] [(count m) (count (first m))])
(defn mcontains? [m [i j]] (let [[h w] (mshape m)] (and (< -1 i h) (< -1 j w))))


(defn mfind [m pred]
  (let [[h w] (mshape m)]
    (for [i (range h), j (range w)
          :when (pred (mget m [i j]))]
      [i j])))


(defn v+ [& vs] (mapv (partial apply +) (apply (partial map vector) vs)))
(defn v- [& vs] (mapv (partial apply -) (apply (partial map vector) vs)))
(defn v*scalar [v x] (mapv #(* x %) v))

