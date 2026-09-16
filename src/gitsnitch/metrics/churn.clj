(ns gitsnitch.metrics.churn
  (:require [gitsnitch.filters :as filters]
            [gitsnitch.util :as util]
            [clojure.string :as str]))

(defn- parent-dir
  "Extract the directory component of a file path."
  [path]
  (let [idx (str/last-index-of path "/")]
    (if idx
      (subs path 0 (inc idx))
      "./")))

;; ---------------------------------------------------------------------------
;; Accumulator API for single-pass summary
;; ---------------------------------------------------------------------------

(defn init-acc
  "Return initial accumulator state for file-churn."
  []
  {:total-commits 0 :files {}})

(defn accumulate-step
  "Accumulate one commit into file-churn state.
   With opts, applies the same :path/:exclude filtering and :detailed?
   numstat tracking as file-churn. Without opts (used by summary's
   single-pass reduce), every file is counted unfiltered."
  ([acc commit] (accumulate-step acc commit nil))
  ([acc commit opts]
   (let [keep? (if opts
                 (fn [path]
                   (and ((filters/path-filter (:path opts)) path)
                        ((filters/exclude-filter (:exclude opts)) path)))
                 (constantly true))
         date  (:commit/author-date commit)
         files (if (:detailed? opts)
                 (:commit/numstat commit)
                 (map (fn [f] {:path f}) (:commit/files commit)))
         total-commits (inc (:total-commits acc))]
     (reduce
      (fn [acc2 file-info]
        (let [path (:path file-info)]
          (if (keep? path)
            (update-in acc2 [:files path]
                       (fn [cur]
                         (let [cur (or cur {:changes 0 :last-changed nil
                                            :insertions 0 :deletions 0})]
                           (cond-> (-> cur
                                       (update :changes inc)
                                       (update :last-changed
                                               (fn [old]
                                                 (if (or (nil? old) (and date (pos? (compare date old))))
                                                   date old))))
                             (:detailed? opts)
                             (-> (update :insertions + (or (:insertions file-info) 0))
                                 (update :deletions + (or (:deletions file-info) 0)))))))
            acc2)))
      (assoc acc :total-commits total-commits)
      files))))

(defn finalize-churn
  "Finalize accumulated state into the standard churn result map."
  [acc]
  (let [total (:total-commits acc)]
    {:total-commits total
     :total-files   (count (:files acc))
     :rows (->> (:files acc)
                (map (fn [[path stats]]
                       {:path         path
                        :changes      (:changes stats)
                        :percent      (util/pct (:changes stats) total)
                        :last-changed (:last-changed stats)}))
                (sort-by (juxt (comp - :changes) :path))
                vec)}))

(defn file-churn
  "Reduce commits into a sorted vector of file churn maps.
   Options:
     :exclude   - coll of glob patterns to exclude
     :path      - coll of path prefixes to include
     :detailed? - if true, expects :commit/numstat on commits"
  [commits opts]
  (let [result (reduce #(accumulate-step %1 %2 opts) (init-acc) commits)
        total  (:total-commits result)]
    {:total-commits total
     :total-files   (count (:files result))
     :rows (->> (:files result)
                (map (fn [[path stats]]
                       (cond-> {:path         path
                                :changes      (:changes stats)
                                :percent      (util/pct (:changes stats) total)
                                :last-changed (:last-changed stats)}
                         (:detailed? opts)
                         (assoc :insertions (:insertions stats)
                                :deletions  (:deletions stats)))))
                (sort-by (juxt (comp - :changes) :path))
                vec)}))

(defn dir-churn
  "Aggregate commits into directory-level churn.
   A single commit touching 3 files under the same dir counts as 1 change."
  [commits opts]
  (let [keep? (fn [path]
                (and ((filters/path-filter (:path opts)) path)
                     ((filters/exclude-filter (:exclude opts)) path)))
        result (reduce
                (fn [acc commit]
                  (let [date  (:commit/author-date commit)
                        files (filterv keep? (:commit/files commit))
                        dirs  (distinct (map parent-dir files))
                        total-commits (inc (:total-commits acc))]
                    (reduce
                     (fn [acc2 dir]
                       (update-in acc2 [:dirs dir]
                                  (fn [cur]
                                    (let [cur (or cur {:commit-count 0
                                                       :file-paths #{}
                                                       :last-changed nil})]
                                      (-> cur
                                          (update :commit-count inc)
                                          (update :file-paths into
                                                  (if (= dir "./")
                                                    files
                                                    (filterv #(str/starts-with? % dir) files)))
                                          (update :last-changed
                                                  (fn [old]
                                                    (if (or (nil? old) (and date (pos? (compare date old))))
                                                      date old))))))))
                     (assoc acc :total-commits total-commits)
                     dirs)))
                {:total-commits 0 :dirs {}}
                commits)
        total (:total-commits result)]
    {:total-commits total
     :total-dirs    (count (:dirs result))
     :rows (->> (:dirs result)
                (map (fn [[dir stats]]
                       {:dir          dir
                        :changes      (:commit-count stats)
                        :file-count   (count (:file-paths stats))
                        :percent      (util/pct (:commit-count stats) total)
                        :last-changed (:last-changed stats)}))
                (sort-by (juxt (comp - :changes) :dir))
                vec)}))
