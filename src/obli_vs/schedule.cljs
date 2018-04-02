(ns obli-vs.schedule
  (:require [clojure.spec.alpha :as s]
            [clojure.spec.gen.alpha :as gen]
            [obli-vs.knapsack :as k]))

(s/def ::name string?)
(s/def ::description string?)

(s/def ::event-props (s/keys :req-un [::name] :opt-un [::description]))
(s/def ::event (s/merge ::k/item ::event-props))

(s/def ::frequency #{:once 
                     :daily
                     :weekdays
                     :weekends
                     :mondays
                     :tuesdays
                     :wednesdays
                     :thursdays
                     :fridays
                     :saturdays
                     :sundays})

(s/def ::start-time inst?) ;; in chunks of 5 min
(s/def ::end-time inst?)

(s/def ::static-event
  (s/keys :req-un [::name ::frequency ::start-time ::end-time]
          :opt-un [::description]))

(s/def ::static (s/coll-of ::static-event))
(s/def ::unplanned (s/coll-of ::event))

(s/def ::block (s/keys :req-un [::k/size ::k/items ::start-time]))
(s/def ::blocks (s/coll-of ::block))
(gen/generate (s/gen ::block))

(s/def ::calendar (s/keys :req-un [::static ::unplanned ::blocks]))
(gen/generate (s/gen ::calendar))

(defn now [] (java.util.Date.))

(defn once? [static-event]
  (= :once (:frequency static-event)))

(defn past? [current-inst static-or-block]
  (> current-inst (:end-time static-or-block)))

(defn comp-and [val & preds]
  (reduce (fn [acc p] (if acc (p val) acc)) true preds))

(defn remove-past [current-inst calendar]
  (let [{:keys [static blocks]} calendar
          past? (partial past? current-inst)
          new-static (remove #(comp-and % once? past?) static)
          new-blocks (remove past? blocks)]
    (merge calendar {:static new-static :blocks new-blocks})))

(defn static-event->sack [event]
  (let [{:keys [start-time end-time]} event
        size (- end-time start-time event)
        item (merge event {:size size})]
    {:size size
     :items (seq [item])
     :start-time start-time}))

(defn extract-once [calendar]
  (let [{events :static} calendar]
    (->> (seq events)
      (filter #(= :once (:frequency %)))
      (map static-event->sack))))
      
(extract-once (gen/generate (s/gen ::calendar)))

(defn calendar->static-chunks [current-inst calendar]
  (let [{events :static} calendar]))
  
