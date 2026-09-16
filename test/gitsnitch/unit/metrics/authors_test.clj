(ns gitsnitch.unit.metrics.authors-test
  (:require [clojure.test :refer [deftest is testing]]
            [gitsnitch.metrics.authors :as authors]
            [gitsnitch.fixtures :as fx]))

(defn- commits []
  [(fx/commit-map {:commit/author-email "a@x.com" :commit/author-name "Alice"
                   :commit/author-date "2024-01-01T00:00:00Z"})
   (fx/commit-map {:commit/author-email "a@x.com" :commit/author-name "Alice"
                   :commit/author-date "2024-01-02T00:00:00Z"})
   (fx/commit-map {:commit/author-email "b@x.com" :commit/author-name "Bob"
                   :commit/author-date "2024-01-03T00:00:00Z"})])

(deftest author-stats-totals-and-ranking
  (let [result (authors/author-stats (commits) {})]
    (is (= 3 (:total-commits result)))
    (is (= 2 (:total-authors result)))
    (is (= ["Alice" "Bob"] (map :author (:rows result))))
    (is (= 2 (:commits (first (:rows result)))))))

(deftest author-stats-first-seen-last-seen
  (let [result (authors/author-stats (commits) {})
        alice (first (:rows result))]
    (is (= "2024-01-01T00:00:00Z" (:first-seen alice)))
    (is (= "2024-01-02T00:00:00Z" (:last-seen alice)))))

(deftest author-stats-path-filter
  (let [commits [(fx/commit-map {:commit/author-email "a@x.com" :commit/files ["src/a.clj"]})
                 (fx/commit-map {:commit/author-email "b@x.com" :commit/files ["docs/readme.md"]})]
        result (authors/author-stats commits {:path ["src/"]})]
    (is (= 1 (:total-commits result)))
    (is (= 1 (:total-authors result)))))

(deftest concentration-warnings-severe-single-author
  (let [rows [{:author "Alice" :commits 8 :percent 80.0}
              {:author "Bob" :commits 2 :percent 20.0}]
        warnings (authors/concentration-warnings rows 10)]
    (is (= :severe (:level (first warnings))))
    (is (re-find #"Alice" (:message (first warnings))))))

(deftest concentration-warnings-top3-warning
  (let [rows [{:author "A" :commits 3 :percent 30.0}
              {:author "B" :commits 3 :percent 30.0}
              {:author "C" :commits 3 :percent 30.0}
              {:author "D" :commits 1 :percent 10.0}]
        warnings (authors/concentration-warnings rows 10)]
    (is (some #(= :warning (:level %)) warnings))))

(deftest concentration-warnings-none-when-balanced
  (let [rows [{:author "A" :commits 5 :percent 50.0}
              {:author "B" :commits 5 :percent 50.0}]]
    (is (empty? (authors/concentration-warnings rows 10)))))

(deftest concentration-warnings-empty-rows
  (is (empty? (authors/concentration-warnings [] 0))))
