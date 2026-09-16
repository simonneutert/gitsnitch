(ns gitsnitch.unit.metrics.activity-test
  (:require [clojure.test :refer [deftest is testing]]
            [gitsnitch.metrics.activity :as activity]
            [gitsnitch.fixtures :as fx]))

(deftest activity-buckets-by-month
  (let [commits [(fx/commit-map {:commit/author-date "2024-01-05T10:00:00Z"})
                 (fx/commit-map {:commit/author-date "2024-01-20T10:00:00Z"})
                 (fx/commit-map {:commit/author-date "2024-02-01T10:00:00Z"})]
        result (activity/activity-buckets commits {:by "month"})]
    (is (= 3 (:total-commits result)))
    (is (= [{:period "2024-01" :commits 2} {:period "2024-02" :commits 1}]
           (:rows result)))))

(deftest activity-buckets-by-day
  (let [commits [(fx/commit-map {:commit/author-date "2024-01-05T10:00:00Z"})
                 (fx/commit-map {:commit/author-date "2024-01-05T22:00:00Z"})]
        result (activity/activity-buckets commits {:by "day"})]
    (is (= [{:period "2024-01-05" :commits 2}] (:rows result)))))

(deftest activity-buckets-by-week-iso
  (let [commits [(fx/commit-map {:commit/author-date "2024-01-01T10:00:00Z"})]
        result (activity/activity-buckets commits {:by "week"})]
    (testing "week bucket uses ISO-week format YYYY-Www"
      (is (re-matches #"\d{4}-W\d{2}" (:period (first (:rows result))))))))

(deftest activity-buckets-defaults-to-month
  (let [commits [(fx/commit-map {:commit/author-date "2024-03-15T10:00:00Z"})]]
    (is (= (activity/activity-buckets commits {})
           (activity/activity-buckets commits {:by "month"})))))

(deftest trend-summary-needs-at-least-4-rows
  (is (nil? (activity/trend-summary [{:period "1" :commits 1}
                                     {:period "2" :commits 1}]))))

(deftest trend-summary-accelerating
  (let [rows [{:period "1" :commits 1} {:period "2" :commits 1}
              {:period "3" :commits 10} {:period "4" :commits 10}]]
    (is (= :accelerating (activity/trend-summary rows)))))

(deftest trend-summary-declining
  (let [rows [{:period "1" :commits 10} {:period "2" :commits 10}
              {:period "3" :commits 1} {:period "4" :commits 1}]]
    (is (= :declining (activity/trend-summary rows)))))

(deftest trend-summary-stable
  (let [rows [{:period "1" :commits 5} {:period "2" :commits 5}
              {:period "3" :commits 5} {:period "4" :commits 5}]]
    (is (= :stable (activity/trend-summary rows)))))
