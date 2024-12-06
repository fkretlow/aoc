(ns aoc.lib.seq)


(defn map-first
  "Return the result of applying `f` to the first element of `coll`
  cons'ed onto the rest of `coll`."
  [f coll]
  (when (seq coll) (cons (f (first coll)) (rest coll))))


(defn pad-end
  "Pad `coll` with `x` until it has `n` elements."
  [n x coll]
  (take n (concat coll (repeat x))))


(defn window-indices
  "Generate a lazy seq of all \"windows\" with the given `radius` around the elements of a
  vector or array with the given `length`, in the form `[i start end]`, end non-inclusive.
  
  Example:
  `1 3` -> `[0 0 2] [1 0 3] [2 1 3]`"
  [radius length]
  (for [i (range length)] [i (max (- i radius) 0) (min (+ i (inc radius)) length)]))


(defn take-until
  "Like take-while, but includes the first element for which pred is true."
  [pred coll]
  (lazy-seq
   (when-let [s (seq coll)]
     (if (pred (first s))
       (cons (first s) nil)
       (cons (first s) (take-until pred (rest s)))))))


(defn drop-until
  "Like drop-while, but also drops the first element for which pred is true."
  [pred coll]
  (lazy-seq
   (when-let [s (seq coll)]
     (if (pred (first s))
       (rest s)
       (drop-until pred (rest coll))))))


(defn drop-nth
  "Drop the nth element of a collection."
  [n coll]
  (keep-indexed (fn [i x] (when-not (= n i) x)) coll))


(defn find-first
  "Find the first element of coll that satisfies pred."
  [pred coll]
  (first (filter pred coll)))


(defn all-pairs
  "Return a seq of all distinct unordered pairs of elements of coll."
  [coll]
  (let [v (if (vector? coll) coll (vec coll))]
    (for [i (range (count v))
          j (range (inc i) (count v))]
      [(v i) (v j)])))


(defn one?
  "Like some? but returns true only when exactly one element of coll matches pred."
  [pred coll]
  (loop [found? false
         coll coll]
    (if (seq coll)
      (if (pred (first coll))
        (if found? false (recur true (rest coll)))
        (recur found? (rest coll)))
      found?)))


(defn min-of-nilable [& xs]
  (apply min (remove nil? xs)))


(defn mapvals
  "Apply f to all the values in m."
  [f m]
  (into {} (for [[k v] m] [k (f v)])))


(defn count-unique
  "Given a seq of elements, count how often each unique element appears in the seq
  and return the results as a map."
  [xs]
  (reduce (fn [counts x] (update counts x #(if % (inc %) 1))) {} xs))


(defn join-connected
  "Given a predicate `connected?` that decides whether two consecutive elements of `coll`
  are connected, split the collection into vectors of consecutive connected elements."
  [connected? coll]
  (loop [segments []
         segment []
         [cur & more] coll]
    (if (some? cur)
      (if (or (empty? segment) (connected? (peek segment) cur))
        (recur segments (conj segment cur) more)
        (recur (conj segments segment) [cur] more))
      (conj segments segment))))


(defn keys-with-min-values
  "Given a map `m`, return a seq of all keys of `m` whose value
  is minimal among all values of `m`."
  [m]
  (second
   (reduce (fn [[min, :as acc] [k v]]
             (cond (< v min) [v [k]]
                   (= v min) (update acc 1 conj k)
                   (> v min) acc))
           (let [[k v] (first m)] [v [k]])
           (rest m))))
