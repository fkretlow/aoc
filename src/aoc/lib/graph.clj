(ns aoc.lib.graph
  (:require
   [clojure.data.priority-map :refer [priority-map-by]]
   [clojure.set :as set]
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


(defn all-shortest-paths
  "Given a vertex `v0` to start from and a function `edgefn` that maps to
  each vertex _v_ a seq of edges of the form _(u,w)_ where _u_ is a neighbor
  of _v_ and the edge has weight _w_, find paths with minimum sums of edge
  weights from `v0` to any reachable vertex using Dijkstra's shortest
  path algorithm."
  [v0 edgefn]
  (loop [pm (priority-map-by
             (fn [[done?_a d_0a] [done?_b d_0b]]
               (cond (= done?_a done?_b) (< d_0a d_0b), done?_a false, :else true))
             v0 [false 0 nil])]
    (let [[v [done? d_0v]] (peek pm)]
      (if done?
        (into {} (for [[v [_ _ parent]] pm]
                   (loop [path [v], parent parent]
                     (if-not parent
                       [v path]
                       (recur (cons parent path)
                              (get-in pm [parent 2]))))))
        (recur (reduce (fn [pm [u d_vu]]
                         (if (contains? pm u)
                           (update pm u (fn [[done? d_0u, :as previous]]
                                          (if (< (+ d_0v d_vu) d_0u)
                                            [done? (+ d_0v d_vu) v]
                                            previous)))
                           (assoc pm u [false (+ d_0v d_vu) v])))
                       (assoc-in pm [v 0] true)
                       (edgefn v)))))))


(defn shortest-path
  "Given two vertices `start` and `end` and a function `edgefn` that maps to
  each vertex _v_ a seq of edges of the form _(u,w)_ where _u_ is a neighbor
  of _v_ and the edge has weight _w_, find one path through the graph with
  minimum sum of edge weights from `start` to `end` using Dijkstra's shortest
  path algorithm."
  [start end edgefn]
  (loop [pm (priority-map-by
             (fn [[done?_a d_0a] [done?_b d_0b]]
               (cond (= done?_a done?_b) (< d_0a d_0b) done?_a false, :else true))
             start [false 0 nil])]
    (if (contains? pm end)
      (loop [[v, :as path] (list end)]
        (let [[_ _ parent] (get pm v)]
          (if parent (recur (cons parent path)) path)))
      (let [[v [done? d_0v]] (peek pm)]
        (when-not done?
          (recur (reduce (fn [pm [u d_vu]]
                           (if (contains? pm u)
                             (update pm u (fn [[done? d_0u, :as previous]]
                                            (if (< (+ d_0v d_vu) d_0u)
                                              [done? (+ d_0v d_vu) v]
                                              previous)))
                             (assoc pm u [false (+ d_0v d_vu) v])))
                         (assoc-in pm [v 0] true)
                         (edgefn v))))))))


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


(defn remove-vertex
  "Given an undirected graph `G` as a map of adjacency sets, and a vertex `v`,
  remove `v` from `G`."
  [G v]
  (reduce (fn [G u] (update G u #(disj % v))) (dissoc G v) (G v)))


(defn find-triangles
  "Given an undirected graph `G` as map of adjacency sets, find all triangles
  (cliques of 3 interconnected vertices) in `G`."
  [G]
  (loop [G G,
         Q (->> (for [[v edges] G,
                      :let [c (count edges)],
                      :when (< 1 c)] [v c])
                (sort-by second >)
                (map first)),
         triangles []]
    (if (empty? Q)
      triangles
      (let [[v & Q'] Q,
            us (G v),
            G' (remove-vertex G v),
            triangles' (->> (loop [us us, pairs []]
                              (if (empty? us)
                                pairs
                                (let [u (first us)
                                      us' (disj us u)
                                      ws (G u)
                                      pairs' (->> (set/intersection us' ws)
                                                  (map (fn [w] [u w]))
                                                  (apply conj pairs))]
                                  (recur us' pairs'))))
                            (map #(conj % v))
                            (apply conj triangles))]
        (recur G' Q' triangles')))))


(defn find-max-cliques
  "Given an undirected graph `G` as a map of adjacency sets, find all maximal
  cliques (sets of interconnected vertices) in `G` using the Bron-Kerbosch
  algorithm."
  [G]
  ((fn -bron-kerbosch [R P X]
     (if (and (empty? P) (empty? X))
       (when (seq R) [R])
       (loop [P P,
              X X,
              cliques []]
         (if (empty? P)
           cliques
           (let [v (first P), Nv (G v)]
             (recur (disj P v)
                    (conj X v)
                    (apply conj cliques (-bron-kerbosch (conj R v)
                                                        (set/intersection P Nv)
                                                        (set/intersection X Nv)))))))))
   #{} (set (keys G)) #{}))

