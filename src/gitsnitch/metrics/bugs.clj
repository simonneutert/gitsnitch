(ns gitsnitch.metrics.bugs
  (:require [gitsnitch.filters :as filters]
            [clojure.string :as str]))

(defn- bug-match?
  "Return true if the commit subject matches the bug-related regex pattern."
  [subject pattern]
  (boolean (re-find pattern (str/lower-case (or subject "")))))

(defn bug-hotspots
  "Reduce commits into a sorted vector of bug-hotspot file maps.
   Commits whose subject matches the grep pattern are counted.
   Options:
     :grep    - regex pattern string for bug keywords
     :path    - coll of path prefixes to include
     :exclude - coll of glob patterns to exclude"
  [commits opts]
  (let [grep-str (or (:grep opts) "fix|bug|broken|defect|issue|repair|patch")
        pat      (re-pattern (str "(?i)" grep-str))
        keep?    (fn [path]
                   (and ((filters/path-filter (:path opts)) path)
                        ((filters/exclude-filter (:exclude opts)) path)))
        result   (reduce
                  (fn [acc commit]
                    (let [total-commits (inc (:total-commits acc))
                          subj (:commit/subject commit)]
                      (if (bug-match? subj pat)
                        (let [files (filterv keep? (:commit/files commit))]
                          (reduce
                           (fn [acc2 path]
                             (update-in acc2 [:files path]
                                        (fn [cur]
                                          (let [cur (or cur {:bug-changes 0 :last-bug nil})]
                                            (-> cur
                                                (update :bug-changes inc)
                                                (update :last-bug
                                                        (fn [old]
                                                          (let [date (:commit/author-date commit)]
                                                            (if (or (nil? old) (and date (pos? (compare date old))))
                                                              date old)))))))))
                           (-> acc
                               (assoc :total-commits total-commits)
                               (update :bug-commits inc))
                           files))
                        (assoc acc :total-commits total-commits))))
                  {:total-commits 0 :bug-commits 0 :files {}}
                  commits)
        bug-total (:bug-commits result)]
    {:total-commits (:total-commits result)
     :bug-commits   bug-total
     :total-files   (count (:files result))
     :rows (->> (:files result)
                (map (fn [[path stats]]
                       {:path        path
                        :bug-changes (:bug-changes stats)
                        :percent     (if (pos? bug-total)
                                       (Double/parseDouble
                                        (format "%.1f" (* 100.0 (/ (:bug-changes stats) bug-total))))
                                       0.0)
                        :last-bug    (:last-bug stats)}))
                (sort-by (juxt (comp - :bug-changes) :path))
                vec)}))
