(ns gitsnitch.unit.metrics.coupling-test
  (:require [clojure.test :refer [deftest is testing]]
            [gitsnitch.metrics.coupling :as coupling]
            [gitsnitch.fixtures :as fx]))

(deftest coupling-stats-counts-cochanges
  (let [commits [(fx/commit-map {:commit/files ["a.clj" "b.clj"]})
                 (fx/commit-map {:commit/files ["a.clj" "b.clj"]})]
        result (coupling/coupling-stats commits {:min-cochanges 1})]
    (is (= 1 (:total-pairs result)))
    (is (= 1 (count (:rows result))))
    (let [row (first (:rows result))]
      (is (= "a.clj" (:file-a row)))
      (is (= "b.clj" (:file-b row)))
      (is (= 2 (:cochanges row))))))

(deftest coupling-stats-min-cochanges-filters-rows
  (let [commits [(fx/commit-map {:commit/files ["a.clj" "b.clj"]})]
        result (coupling/coupling-stats commits {:min-cochanges 2})]
    (is (empty? (:rows result)))
    (testing "pair still exists internally, just below the display threshold"
      (is (= 1 (:total-pairs result))))))

(deftest coupling-stats-skips-oversized-commits
  (let [big-commit (fx/commit-map {:commit/files (mapv #(str "f" % ".clj") (range 10))})
        result (coupling/coupling-stats [big-commit] {:max-files-per-commit 5})]
    (is (= 1 (:skipped-commits result)))
    (is (empty? (:rows result)))))

(deftest coupling-stats-single-file-commit-has-no-pairs
  (let [result (coupling/coupling-stats [(fx/commit-map {:commit/files ["a.clj"]})] {})]
    (is (= 0 (:total-pairs result)))))

(deftest coupling-stats-jaccard-and-confidence-bounds
  (let [commits [(fx/commit-map {:commit/files ["a.clj" "b.clj"]})
                 (fx/commit-map {:commit/files ["a.clj"]})]
        result (coupling/coupling-stats commits {:min-cochanges 1})
        row (first (:rows result))]
    (is (<= 0.0 (:jaccard row) 1.0))
    (is (<= 0.0 (:confidence-ab row) 100.0))
    (is (<= 0.0 (:confidence-ba row) 100.0))))
