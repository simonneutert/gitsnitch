(ns gitsnitch.metrics.coupling
  (:require [gitsnitch.filters :as filters]
            [gitsnitch.util :as util]))

(defn- file-pairs
  "Generate all unordered pairs from a collection of file paths.
   Returns a seq of [a b] where a < b lexicographically."
  [files]
  (let [sorted (sort files)
        v      (vec sorted)
        n      (count v)]
    (for [i (range n)
          j (range (inc i) n)]
      [(v i) (v j)])))

(defn coupling-stats
  "Reduce commits into file-pair co-change statistics.

   Options:
     :path               - coll of path prefixes to include
     :exclude            - coll of glob patterns to exclude
     :max-files-per-commit - skip commits touching more than N files (default: 50)
     :min-cochanges      - minimum co-change count to include in results (default: 2)

   Returns:
     {:total-commits n
      :skipped-commits n  (commits exceeding max-files-per-commit)
      :total-pairs n
      :rows [{:file-a :file-b :cochanges :support :confidence-ab :confidence-ba :jaccard}]}"
  [commits opts]
  (let [max-files    (or (:max-files-per-commit opts) 50)
        min-cochange (or (:min-cochanges opts) 2)
        keep?        (fn [path]
                       (and ((filters/path-filter (:path opts)) path)
                            ((filters/exclude-filter (:exclude opts)) path)))
        result (reduce
                (fn [acc commit]
                  (let [files (filterv keep? (:commit/files commit))]
                    (if (> (count files) max-files)
                      ;; Skip oversized commits to avoid quadratic explosion
                      (-> acc
                          (update :total-commits inc)
                          (update :skipped-commits inc))
                      (let [pairs (file-pairs files)]
                        (reduce
                         (fn [acc2 [a b]]
                           (-> acc2
                               (update-in [:pairs [a b]] (fnil inc 0))))
                         (-> acc
                             (update :total-commits inc)
                             ;; Track per-file commit counts for confidence metrics
                             (update :file-counts
                                     (fn [fc]
                                       (reduce (fn [m f] (update m f (fnil inc 0)))
                                               fc files))))
                         pairs)))))
                {:total-commits 0 :skipped-commits 0 :pairs {} :file-counts {}}
                commits)
        file-counts (:file-counts result)
        total       (:total-commits result)]
    {:total-commits   total
     :skipped-commits (:skipped-commits result)
     :total-pairs     (count (:pairs result))
     :rows (->> (:pairs result)
                (filter (fn [[_pair cnt]] (>= cnt min-cochange)))
                (map (fn [[[a b] cnt]]
                       (let [count-a (get file-counts a 0)
                             count-b (get file-counts b 0)
                             union   (- (+ count-a count-b) cnt)]
                         {:file-a        a
                          :file-b        b
                          :cochanges     cnt
                          :support       (util/pct cnt union "%.2f")
                          :confidence-ab (util/pct cnt count-a "%.0f")
                          :confidence-ba (util/pct cnt count-b "%.0f")
                          :jaccard       (if (pos? union)
                                           (Double/parseDouble
                                            (format "%.2f" (double (/ cnt union))))
                                           0.0)})))
                (sort-by (juxt (comp - :cochanges) :file-a :file-b))
                vec)}))
