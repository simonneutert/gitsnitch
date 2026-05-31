(ns gitsnitch.metrics.authors
  (:require [clojure.string :as str]
            [gitsnitch.filters :as filters]))

;; ---------------------------------------------------------------------------
;; Accumulator API for single-pass summary
;; ---------------------------------------------------------------------------

(defn init-acc
  "Return initial accumulator state for author-stats."
  []
  {:total 0 :authors {}})

(defn- step-author
  "Update author accumulator state with one commit.
   Keys by author-email; tracks the most-recently-seen author-name."
  [acc commit]
  (let [email  (:commit/author-email commit)
        name   (:commit/author-name commit)
        date   (:commit/author-date commit)]
    (-> acc
        (update :total inc)
        (update-in [:authors email]
                   (fn [cur]
                     (let [cur (or cur {:commits 0
                                        :name name
                                        :first-seen date
                                        :last-seen date})]
                       (-> cur
                           (update :commits inc)
                           (update :first-seen
                                   (fn [old]
                                     (if (and old date (neg? (compare old date)))
                                       old date)))
                           (update :last-seen
                                   (fn [old]
                                     (if (and old date (pos? (compare old date)))
                                       old date)))
                           ;; Update name to most recent
                           (cond->
                            (and date (:last-seen cur)
                                 (not (neg? (compare date (:last-seen cur)))))
                             (assoc :name name)))))))))

(def accumulate-step step-author)

(defn finalize-authors
  "Finalize accumulated state into the standard author-stats result map."
  [acc]
  (let [total (:total acc)]
    {:total-commits total
     :total-authors (count (:authors acc))
     :rows (->> (:authors acc)
                (map (fn [[_email stats]]
                       {:author     (:name stats)
                        :commits    (:commits stats)
                        :percent    (if (pos? total)
                                      (Double/parseDouble
                                       (format "%.1f" (* 100.0 (/ (:commits stats) total))))
                                      0.0)
                        :first-seen (:first-seen stats)
                        :last-seen  (:last-seen stats)}))
                (sort-by (comp - :commits))
                vec)}))

(defn author-stats
  "Reduce commits into author statistics.
   When :path is set, only commits touching at least one matching file are counted.
   Returns {:total-commits n :rows [{:author :commits :percent :first-seen :last-seen}]}"
  [commits opts]
  (finalize-authors (reduce accumulate-step (init-acc)
                            (filters/filter-commits-by-path commits (:path opts)))))

(defn concentration-warnings
  "Produce bus-factor warnings from author stats rows."
  [rows _total-commits]
  (let [top1-pct  (when (seq rows) (:percent (first rows)))
        top3-pct  (when (>= (count rows) 3)
                    (reduce + (map :percent (take 3 rows))))]
    (cond-> []
      (and top1-pct (> top1-pct 60.0))
      (conj {:level :severe
             :message (str "Single author (" (:author (first rows))
                           ") has " top1-pct "% of commits — severe concentration risk")})

      (and top3-pct (> top3-pct 80.0))
      (conj {:level :warning
             :message (str "Top 3 authors account for " (format "%.1f" top3-pct)
                           "% of commits — strong concentration warning")}))))
