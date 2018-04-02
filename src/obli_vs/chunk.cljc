(ns obli-vs.chunk)

(defn- overlap?
  "Check if there's any overlap in ranges"
  [c0 c1]
  (> (last c0) (first c1)))

(defn any-overlap?
  "Check if any ranges overlap in collection"
  [coll]
  (loop [item (first coll)
         tail (rest coll)
         acc false]
    (if (and (not acc) (pos? (count tail)))
      (recur
        (first tail)
        (rest tail)
        (reduce #(if %1 %1 (overlap? item %2)) false tail))
      acc)))

(defn- concat-chunk [iso acc start end item]
  (let [{:keys [to from]} iso
        [item-start item-end] (to item)]
    (cond
      (= start item-start) (concat acc [item])
      (< start item-start item-end end) (concat acc [(from start item-start)] [item])
      (< start end) (concat acc [(from start end)])
      :default acc)))

;; doesn't check for overlaps and doesn't sort coll
(defn- chunk-range
  [iso start end coll]
  (let [{:keys [to from]} iso]
    (loop [start start
           item (first coll)
           tail (rest coll)
           acc '()]
      (if (and (< start end) (not (nil? item)))
        (recur
          (last (to item))
          (first tail)
          (rest tail)
          (concat-chunk iso acc start end item))
        (if (>= start end)
          acc
          (concat acc [(from start end)]))))))

(defn- sort-items [iso coll]
  (let [{:keys [to]} iso]
    (sort-by (comp first to) coll)))

(defn split-chunks 
  "Turn a range into chunks with items in between.
  Takes an isomorphic relation that defines how to turn
  a start and end pair into an item (to), and vice versa (from).

  ex: {:to (fn [item] [(first item) (last item)])
       :from (fn [start end] (conj [start] end))}"
  [iso start end coll]
  (let [sorted (sort-items iso coll)]
    (if (not (any-overlap? (map (:to iso) sorted)))
      (chunk-range iso start end sorted)
      (throw (AssertionError. "Overlapping ranges in collection")))))


