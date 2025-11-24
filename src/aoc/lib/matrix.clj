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

(defn- array? [x] (.isArray (class x)))

(defn- mtype
  "Determine the underlying type of the (supposed) matrix _m_.
  Throws an `UnsupportedOperationException` if _m_ is not one of:
  - a 2d array
  - a nested sequential (i.e. native clojure vectors, lists or seqs)"
  [m]
  (cond
    (and (array? m) (array? (first m))) :array
    (and (sequential? m) (sequential? (first m))) :native
    :else (throw (UnsupportedOperationException. (format "not supported as a matrix type: %s" (type m))))))

(defn mget [m [i j]]
  (case (mtype m)
    :array (aget m i j)
    :native (get-in m [i j])))

(defn mset [m [i j] x]
  (case (mtype m)
    :array (doto m (aset i j x))
    :native (assoc-in m [i j] x)))

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

(defn m* [m1 m2]
  (let [[h1 w1] (mshape m1)
        [h2 w2] (mshape m2)]
    (assert (= w1 h2) (format "cannot multiply matrices of shapes %dx%d and %dx%d", h1 w1 h2 w2))
    (->> (for [i (range h1), j (range w2)]
           (reduce + (for [r (range w1)] (* (get-in m1 [i r]) (get-in m2 [r j])))))
         (partition w2)
         (mapv vec))))
