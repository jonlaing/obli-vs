(ns obli-vs.schedule
  (:require [clojure.spec.alpha :as s]
            [clojure.spec.gen.alpha :as gen]
            [obli-vs.knapsack :as k]))

(s/def ::name string?)
(s/def ::description string?)

(s/def ::event-props (s/keys :req-un [::name] :opt-un [::description]))
(s/def ::event (s/merge ::k/item ::event-props))

(s/def ::frequency #{:daily
                     :weekdays
                     :weekends
                     :mondays
                     :tuesdays
                     :wednesdays
                     :thursdays
                     :fridays
                     :saturdays
                     :sundays})

(s/def ::start-time (s/int-in 0 289)) ;; in chunks of 5 min
(s/def ::end-time (s/int-in 0 289))

(s/def ::static-event
  (s/keys :req-un [::name ::frequency ::start-time ::end-time]
          :opt-un [::description]))

(s/def ::static (s/coll-of ::static-event))
(s/def ::dynamic (s/coll-of ::event))

(s/def ::calendar (s/keys :req-un [::static ::dynamic]))
(gen/generate (s/gen ::calendar))