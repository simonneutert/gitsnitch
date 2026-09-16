(ns gitsnitch.metrics.danger
  (:require [clojure.string :as str]
            [gitsnitch.filters :as filters]))

(def ^:private default-pattern "revert|hotfix|rollback|emergency|workaround|hack")

(defn danger-stats
  "One-pass reduce over commits. For each commit whose subject matches the
   grep pattern, accumulate:
     - total matching commits
     - counts by file touched
     - counts by author
     - counts by month (for firefighting density)
   Options:
     :grep - regex pattern string
     :path - coll of path prefixes to include (scopes :by-file only)
     :top  - number of results per section"
  [commits opts]
  (let [grep-str  (or (:grep opts) default-pattern)
        pat       (re-pattern (str "(?i)" grep-str))
        keep-file? (filters/path-filter (:path opts))
        result    (reduce
                   (fn [acc commit]
                     (let [total (inc (:total-commits acc))
                           subj  (:commit/subject commit)]
                       (if (re-find pat (str/lower-case (or subj "")))
                         (let [author      (:commit/author-name commit)
                               date        (:commit/author-date commit)
                               month       (when (and date (>= (count date) 7))
                                             (subs date 0 7))
                               files       (filterv keep-file? (:commit/files commit))]
                           (-> acc
                               (assoc :total-commits total)
                               (update :danger-commits inc)
                               (update :by-author
                                       #(update % author (fnil inc 0)))
                               (update :by-month
                                       #(if month (update % month (fnil inc 0)) %))
                               (update :by-file
                                       (fn [m] (reduce #(update %1 %2 (fnil inc 0)) m files)))
                               (update :recent-commits
                                       #(conj % {:short   (:commit/short commit)
                                                 :subject subj
                                                 :author  author
                                                 :date    date}))))
                         (assoc acc :total-commits total))))
                   {:total-commits  0
                    :danger-commits 0
                    :by-author      {}
                    :by-month       {}
                    :by-file        {}
                    :recent-commits []}
                   commits)
        top-n     (or (:top opts) 20)
        sort-desc (fn [m] (->> m (sort-by val >) vec))]
    {:total-commits  (:total-commits result)
     :danger-commits (:danger-commits result)
     :by-author      (->> (sort-desc (:by-author result)) (take top-n) vec)
     :by-file        (->> (sort-desc (:by-file result)) (take top-n) vec)
     :by-month       (->> (:by-month result) (sort-by key) vec)
     :recent-commits (->> (:recent-commits result) (take-last 10) reverse vec)}))
