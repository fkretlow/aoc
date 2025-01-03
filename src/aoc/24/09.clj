(ns aoc.24.09
  (:require
   [aoc.lib.io :refer [get-puzzle-input]]
   [data.deque :refer [add-first add-last deque peek-first peek-last
                       remove-first remove-last]]))

(defn parse
  "Parse a string of digits into a sequence of blocks of the form `[width id-or-nil]`.
  Returns the blocks as a deque for convenient further processing."
  [input]
  (loop [d (deque)
         lengths (map (comp parse-long str) (seq input))
         is-file? true
         file-id 0]
    (if (seq lengths)
      (recur (add-last d [(first lengths) (when is-file? file-id)])
             (rest lengths)
             (not is-file?)
             (cond-> file-id is-file? inc))
      d)))

(defn- is-file? [[_ content]] (some? content))

(defn move-one-block [[disk disk']]
  (let [[w-left :as block-left] (peek-first disk)
        [w-right content-right, :as block-right] (peek-last disk)]
    (cond
      (is-file? block-left)
      [(remove-first disk) (conj disk' block-left)]

      (not (is-file? block-right))
      [(remove-last disk) disk']

      (< w-left w-right)
      [(-> disk
           (remove-first)
           (remove-last)
           (add-last [(- w-right w-left) content-right]))
       (conj disk' [w-left content-right])]

      (= w-left w-right)
      [(-> disk (remove-first) (remove-last)) (conj disk' block-right)]

      (> w-left w-right)
      [(-> disk
           (remove-first)
           (add-first [(- w-left w-right) nil])
           (remove-last))
       (conj disk' block-right)])))

(defn part-1 [disk]
  (let [disk' (loop [[disk disk', :as state] [disk []]]
                (if (seq disk)
                  (recur (move-one-block state))
                  disk'))]
    (->> disk'
         (mapcat (fn [[w id]] (repeat w id)))
         (map-indexed (fn [i id] (* i id)))
         (reduce +))))

(let [disk (parse (get-puzzle-input 24 9))]
  (println "Part 1:" (time (part-1 disk))))

;; Part 2 is a complete separate implementation this time...

(defn- parse-disk-state [input]
  (loop [i 0,
         files (transient []),
         free-blocks {},
         disk (transient []),
         lengths (->> (seq input)
                      (mapv (comp parse-long str))
                      (mapv vector (interpose nil (range))))]
    (if (empty? lengths)
      {:files (persistent! files), :free-blocks free-blocks, :disk (persistent! disk)}
      (let [[[id l]] lengths]
        (recur (+ i l)
               (cond-> files id (conj! [id i l]))
               (cond-> free-blocks (not id) (update l #(conj (or % (sorted-set)) i)))
               (reduce (fn [disk j] (assoc! disk j id)) disk (range i (+ i l)))
               (rest lengths))))))

(defn- find-leftmost-free-block
  "Given a map of lengths to positions of free blocks and a minimum length, return a pair of the
  form `[length position]` corresponding to the leftmost free block that is large enough."
  [free-blocks i_max l_min]
  (->> (for [[l is] free-blocks,
             :let [i (first is)]
             :when (and i (<= l_min l) (< i i_max))]
         [i l])
       (sort-by first)
       (first)))

(defn- move-file
  "Given the current state of the disk, attempt to move the rightmost unprocessed file as
  far to the left as possible. Regardless of whether the move operation succeeded, return
  the new disk state with the rightmost file popped from the stack."
  [{:keys [files free-blocks], :as disk-state}]
  (let [[id_f i_f l_f] (peek files)]
    (if-let [[i_b l_b] (find-leftmost-free-block free-blocks i_f l_f)]
      (-> disk-state
          ; Remove the processed file.
          (update :files pop)
          ; Move the file by writing its id to the new position on the disk and clearing the old position.
          (update :disk #(reduce (fn [disk j] (assoc disk j nil)) % (range i_f (+ i_f l_f))))
          (update :disk #(reduce (fn [disk j] (assoc disk j id_f)) % (range i_b (+ i_b l_f))))
          ; Update the available free blocks.
          (update :free-blocks
                  (fn [free-blocks]
                    (cond-> free-blocks
                      true (update l_b #(disj % i_b))
                      (< l_f l_b) (update (- l_b l_f) #(conj % (+ i_b l_f)))))))
      ; Just remove the processed file from the stack.
      (update disk-state :files pop))))

(defn- checksum [disk]
  (->> disk (map #(or % 0)) (map-indexed *) (apply +)))

(defn part-2 [disk-state]
  (->> (loop [state disk-state]
         (if (empty? (:files state))
           (:disk state)
           (recur (move-file state))))
       (checksum)))

(let [state (parse-disk-state (get-puzzle-input 24 9))]
  (println "Part 2:" (time (part-2 state))))

