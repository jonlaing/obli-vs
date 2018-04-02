(defn boolcomp [& preds] 
  (fn [x]
    (loop [pred (first preds)
           tail (rest preds)]
      (cond
        (nil? pred) false
        (pred x) true
        :else (recur (first tail) (rest tail))))))