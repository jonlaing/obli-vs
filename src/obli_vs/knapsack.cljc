(ns obli-vs.knapsack
  (:require [clojure.spec.alpha :as s]))

(s/def ::size integer?)
(s/def ::value integer)

(s/def ::item (s/keys :req [::size ::value]))
(s/def ::items (s/coll-of ::item))
(s/def ::sack (s/keys :req [::size ::items]))

;; (defn best-fit [start max-size items]
;;   (loop [i start:w

;;          size max-size]
;;     (if (pos? i)
;;       (let [item (nth items (- i 1))
;;             item-size (get item :size)
;;             item-value (get item :value)]
;;         (if (> item-size size)
;;           (recur (- i 1) size)
;;           (max
;;             (recur (- i 1) size)
;;             (recur (- i 1) (- size item-size)))))
;;       0 )))
;
;; (defn accept-tuple [accepted rejected] {:accepted accepted :rejected rejected})
;
;; (defn accept [start max-size items items-diff]
;;   (loop [i start]
;;     (if (pos? i)
;;       (let [item (nth items (- i 1))
;;             {:keys [accepted rejected]} items-diff
;;             best-fit-with-index #(best-fit % max-size items)]
;;         (if (= (best-fit-with-index i) (best-fit-with-index (- i 1)))
;;           (recur (- i 1) max-size items (accept-tuple accepted (conj rejected item)))
;;           (recur (- i 1) max-size items (accept-tuple (conj accepted item) rejected))))
;;       items-diff )))
;
;; (defn pack [sack items]
;;   (let [item-count (count items)
;;         empty (empty-space sack)
;;         accepted (accept-tuple [] [])
;;         {:keys [accepted rejected]} (accept item-count empty item accepted)
;;         new-items (concat (get sack :items) accepted)]
;;     [(assoc sack :items new-items) rejected]))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Try again the other way
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn prop-sum [key items] (reduce #(+ %1 (get items key)) 0 items))
(defn sizes [items] (prop-sum :size items))
(defn values [items] (prop-sum :value items))

(defn prop-at [key i items] (get (nth items i) key))
(defn size-at [i items] (prop-at :size i items))
(defn value-at [i items] (prop-at :value i items))

(defn best-fit-table
  "build a table of the best fits per size"
  [items]
  (let [m (atom [(into [] (repeat (sizes items) 0))])]
    (for [i (range (count items))
          j (range (sizes items))]
      (if (> (size-at i items) j)
        (swap! m assoc-in [i j] (get-in m [(- i 1) j]))
        (swap! m assoc-in [i j]
          (max
            (get-in m [(- i 1) j])
            (+
              (value-at i items)
              (get-in m [i (- j (weight-at i j))]))))))
    m))

(defn best-fit [max-size items]
  (loop [table (best-fit-table items)
         max-value 0
         row-index 0
         i 0]
    (if (< row-index (count itmes))
      (let [row (first table)
            max-value-for-row (reduce max row)]
        (if (>= max-value-for-row max-value)
          (recur
            (rest table)
            max-value-for-row
            (+ row-index 1)
            row-index)
          (recur (rest table) max-value (+ row-index 1) i)))
      i))) ;; take this number of items from the list


(defn pack [sack items]
  (let [item-count (count items)
        empty (empty-space sack)
        num-accepted (best-fit empty items)
        accepted (take num-accepted items)
        rejected (drop num-accepted items)
        new-items (concat (get sack :items) accepted)]
    {:sack (assoc sack :items new-items) :rejected rejected}))
