(ns aoc.lib.range)


(defn intersect? [r1 r2] (let [[r1 r2] (sort [r1 r2])] (< (r2 0) (r1 1))))


(defn intersection
  "Given two ranges r1 and r2, return a range whose elements are in both
  r1 and r2, or nil if the two ranges don't intersect."
  [r1 r2]
  (let [[r1 r2] (sort [r1 r2])]
    (when (intersect? r1 r2)
      [(max (r1 0) (r2 0)) (min (r1 1) (r2 1))])))


(defn difference
  "Given two ranges r1 and r2, return a vector of one
  or two ranges whose elements are in r1 but not in r2."
  [r1 r2]
  (if-let [d (intersection r1 r2)]
    (filterv #(< (% 0) (% 1)) [[(r1 0) (d 0)] [(d 1) (r1 1)]])
    [r1]))


(defn contains?
  "Return whether `x` is contained in the range [`start` `end`)."
  [[start end] x]
  (and (<= start x) (< x end)))


(defn shift [d r] [(+ d (r 0)) (+ d (r 1))])
