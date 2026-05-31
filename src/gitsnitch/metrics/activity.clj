(ns gitsnitch.metrics.activity
  (:require [clojure.string :as str]))

(defn- date->bucket
  "Extract a time bucket from an ISO date string."
  [date-str by]
  (when date-str
    (case by
      "month" (subs date-str 0 7)           ;; "2026-04"
      "week"  (let [;; Parse to get ISO week
                    date-str (subs date-str 0 10)] ;; "2026-04-08"
                ;; Approximate: use the Monday of that week
                ;; For simplicity, bucket by YYYY-Www using Java time
                (try
                  (let [ld (java.time.LocalDate/parse date-str)
                        woy (.get ld java.time.temporal.IsoFields/WEEK_OF_WEEK_BASED_YEAR)
                        wby (.get ld java.time.temporal.IsoFields/WEEK_BASED_YEAR)]
                    (format "%d-W%02d" wby woy))
                  (catch Exception _ date-str)))
      "day"   (subs date-str 0 10)          ;; "2026-04-08"
      ;; default to month
      (subs date-str 0 7))))

;; ---------------------------------------------------------------------------
;; Accumulator API for single-pass summary
;; ---------------------------------------------------------------------------

(defn init-acc
  "Return initial accumulator state for activity."
  ([] (init-acc "month"))
  ([by] {:total 0 :buckets (sorted-map) :by by}))

(defn accumulate-step
  "Accumulate one commit into activity state."
  [acc commit]
  (let [bucket (date->bucket (:commit/author-date commit) (:by acc))]
    (-> acc
        (update :total inc)
        (cond->
         bucket (update-in [:buckets bucket] (fnil inc 0))))))

(defn finalize-activity
  "Finalize accumulated state into the standard activity result map."
  [acc]
  {:total-commits (:total acc)
   :rows (->> (:buckets acc)
              (map (fn [[period cnt]]
                     {:period period :commits cnt}))
              vec)})

(defn activity-buckets
  "Reduce commits into time-bucketed activity counts.
   Returns {:total-commits n :rows [{:period :commits}]}"
  [commits {:keys [by] :or {by "month"}}]
  (finalize-activity (reduce accumulate-step (init-acc by) commits)))

(defn trend-summary
  "Simple trend detection: compare first half vs second half of activity."
  [rows]
  (when (>= (count rows) 4)
    (let [mid   (quot (count rows) 2)
          first-half (take mid rows)
          second-half (drop mid rows)
          avg-first  (double (/ (reduce + (map :commits first-half)) (count first-half)))
          avg-second (double (/ (reduce + (map :commits second-half)) (count second-half)))]
      (cond
        (> avg-second (* 1.3 avg-first)) :accelerating
        (< avg-second (* 0.7 avg-first)) :declining
        :else                            :stable))))
