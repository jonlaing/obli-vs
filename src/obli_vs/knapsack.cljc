(ns obli-vs.knapsack
  (:require [clojure.spec.alpha :as s]))

(s/def ::size integer?)
(s/def ::value integer)

(s/def ::item (s/keys :req [::size ::value]))
(s/def ::items (s/coll-of ::item))
(s/def ::sack (s/keys :req [::size ::items]))


(defn prop-sum 
  "Sum of a given property in a list of items"
  [key items]
  (reduce #(+ %1 (get %2 key)) 0 items))

(defn sizes
  "Sum of sizes of a list of items"
  [items]
  (prop-sum :size items))

(defn values
  "Sum of values of a list of items"
  [items]
  (prop-sum :value items))

(defn prop-at
  "Value of a property of an item at a given index"
  [key i items]
  (get (nth items i) key))

(defn size-at
  "Size of an item at a given index"
  [i items]
  (prop-at :size i items))

(defn value-at
  "Value of an item at a given index"
  [i items]
  (prop-at :value i items))

(defn empty-space
  "The empty space in a sack"
  [{:keys [size items]}]
  (- size (sizes items)))

(defn init-table
  "Helper function to create an initial table to build the best-fit table"
  [max-size]
  [(into [] (repeat max-size 0))])

(defn get-from-col [acc i j] (get-in acc [i j]))
(defn get-from-prev-col [acc i j] (get-from-col acc (dec i) j))

(defn best-fit-col
  "Helper function to build a column for the best fit table"
  [max-size acc i items]
  (loop [acc2 [] j 0]
    (if (< j max-size)
      (if (> (size-at (dec i) items) j) 
        (recur (conj acc2 (get-from-prev-col acc i j)) (inc j))
        (recur
          (conj acc2
            (max
              (get-from-prev-col acc i j)
              (+
                (value-at (dec i) items)
                (get-from-col acc (dec i) (- j (size-at (dec i) items))))))
           (inc j)))
      acc2 )))

(defn best-fit-table
  "Build a table of the best fitting items per up to a maximum sack size"
  [max-size items]
  (loop [acc (init-table max-size) i 1]
    (if (< i (count items))
      (recur (conj acc (best-fit-col max-size acc i items)) (inc i))
      acc )))

(defn best-fit
  "Returns the number of items that fit best for a given sack size"
  [max-size items]
  (reduce #(max %1 (nth %2 (dec max-size))) 0 (best-fit-table max-size items)))

(defn conj-key [coll key val]
  (assoc coll key (conj (get coll key) val)))

(defn accept
  "Find the items that best fit into the sack. Returns a map of items that were
  accepted, and those that were rejected."
  [max-size items]
  (let [best (best-fit max-size items)
        j (dec max-size)]
    (loop [i 0 acc {:accept [] :reject []}]
      (if (< i (count items))
        (let [curr-value (+ (values (get acc :accept)) (value-at i items))]
          (if (<= curr-value best)
            (recur (inc i) (conj-key acc :accept (nth items i)))
            (recur (inc i) (conj-key acc :reject (nth items i)))))
        acc ))))

(defn pack
  "Pack best fitting items into a sack"
  [sack items]
  (let [item-count (count items)
        empty (empty-space sack)
        {:keys [accept reject]} (accept empty items)
        new-items (into [] (concat (get sack :items) accept))]
    {:sack (assoc sack :items new-items) :rejected reject}))