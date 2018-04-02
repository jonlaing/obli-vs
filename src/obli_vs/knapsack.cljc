(ns obli-vs.knapsack
  (:require [clojure.spec.alpha :as s]
            [clojure.spec.gen.alpha :as gen]))

(s/def ::size (s/int-in 1 289)) ;; there are 288 chunks of 5 mins in a day
(gen/generate (s/gen ::size))
(s/def ::value #{1 2 3 5 8 13 21}) 

(s/def ::item (s/keys :req-un [::size ::value]))
(s/def ::items (s/coll-of ::item))
(s/def ::sack (s/keys :req-un [::size ::items]))

(defn- prop-sum 
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

(defn- prop-at
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

(defn- init-table
  "Helper function to create an initial table to build the best-fit table"
  [max-size]
  [(into [] (repeat (inc max-size) 0))])

(defn- get-from-col [acc i j] (get-in acc [i j]))
(defn- get-from-prev-col [acc i j] (get-from-col acc (dec i) j))

;; The best-fit algorithm is more-or-less lifted from the Wikipedia page about
;; the Knapsack Problem (https://en.wikipedia.org/wiki/Knapsack_problem).
;; A few modifications needed to be made, and the whole thing also needed
;; to be ported to Clojure.

(defn- best-fit-col
  "Helper function to build a column for the best fit table"
  [max-size acc i items]
  (loop [acc2 [] j 0]
    (if (<= j max-size)
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
      acc2)))

(defn- best-fit-table
  "Build a table of the best fitting items per up to a maximum sack size"
  [max-size items]
  (loop [acc (init-table max-size) i 1] ;; start from 1 because the first col is all 0's
    (if (< i (count items))
      (recur (conj acc (best-fit-col max-size acc i items)) (inc i))
      acc)))

(defn- accept
  "Find the items that best fit into the sack. Returns a map of items that were
  accepted, and those that were rejected."
  [max-size items]
  (let [table (best-fit-table max-size items)]
    (loop [i (dec (count items))
           j max-size
           acc '()]
      (if (and (pos? i) (pos? j))
        (cond
          (= (get-in table [(dec i) j]) (get-in table [i j])) ;; to the left
          (recur (dec i) j acc) ;; move left
          (= (get-in table [i (dec j)]) (get-in table [i j])) ;; above
          (recur i (dec j) acc) ;; move up
          (< (get-in table [i (dec j)]) (get-in table [i j])) ;; above
          (recur i (- j (size-at (dec i) items)) (conj acc (nth items (dec i)))))
        acc))))

(defn- diff
  "Just diffing between two lists. Stolen verbatim from StackOverflow:
  https://stackoverflow.com/questions/23199295/how-to-diff-substract-two-lists-in-clojure"
  [s1 s2]
  (mapcat
    (fn [[x n]] (repeat n x))
    (apply merge-with - (map frequencies [s1 s2]))))

(defn pack
  "Pack best fitting items into a sack. Make sure your items are sorted to ensure
  the best solution. Note: The rejected list will be unsorted."
  [sack items]
  (let [item-count (count items)
        empty (empty-space sack)
        accept (accept empty items)
        reject (diff accept items)
        new-items (concat (:items sack) accept)]
    {:sack (assoc sack :items new-items) :rejected reject}))

(defn map-pack
  "Pack best fitting items into a collection of sacks. Make sure your items are sorted to ensure
  the best solution. You need to provide a sorting comparitor to make sure the items are properly
  sorted after each packing operation."
  [item-sort-by-fn sacks items]
  (loop [sacks sacks
         items items
         acc-sacks '()]
    (if (and (pos? (count sacks)) (pos? (count items)))
      (let [{:keys [sack rejected]} (pack (first sacks) items)]
        (recur (rest sacks) (sort-by item-sort-by-fn rejected) (conj acc-sacks sack)))
      {:sacks acc-sacks :rejected items})))

(let [sack (gen/generate (s/gen ::sack))
      items (gen/generate (s/gen ::items))]
  (map-pack :value sack items))


