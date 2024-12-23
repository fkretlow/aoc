(ns aoc.lib.graph
  (:require
   [clojure.data.priority-map :refer [priority-map-by]]
   [jordanlewis.data.union-find :as uf]))


(defn all-min-distances
  "Given a vertex `v0` to start from and a function `edgefn` that maps to
  each vertex _v_ a seq of edges of the form _(u,w)_ where _u_ is a neighbor
  of _v_ and the edge has weight _w_, find the minimum distance between `v0`
  and any other reacheable vertex in the graph using Dijkstra's shortest path
  algorithm."
  [v0 edgefn]
  (loop [pm (priority-map-by
             (fn [[done?_a d_0a] [done?_b d_0b]]
               (cond (= done?_a done?_b) (< d_0a d_0b), done?_a false, :else true))
             v0 [false 0])]
    (let [[v [done? d_0v]] (peek pm)]
      (if done?
        (reduce (fn [m [v [_ d_0v]]] (assoc m v d_0v)) {} pm)
        (recur (reduce (fn [pm [u d_vu]]
                         (if (contains? pm u)
                           (update-in pm [u 1] #(min % (+ d_0v d_vu)))
                           (assoc pm u [false (+ d_0v d_vu)])))
                       (assoc pm v [true d_0v])
                       (edgefn v)))))))


(defn connected-components
  "Given the set of vertices `V` in a graph and a function `edgefn` that maps
  to each vertex _v_ a seq of edges of the form _(u,...)_ where _u_ is a neighbor
  of _v_, return the connected components of the graph as a union-find structure."
  [V edgefn]
  (loop [V (set V), Q [], dsf (apply uf/union-find V), processed? #{}]
    (if (empty? Q)
      (if (empty? V)
        dsf
        (let [v (first V)]
          (recur (disj V v) (conj Q v) dsf processed?)))
      (let [v (peek Q),
            us (filter #(not (processed? %)) (map first (edgefn v)))]
        (recur V
               (apply conj (pop Q) us)
               (reduce (fn [uf u] (uf/union uf v u)) dsf us)
               (conj processed? v))))))
