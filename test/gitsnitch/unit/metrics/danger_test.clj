(ns gitsnitch.unit.metrics.danger-test
  (:require [clojure.test :refer [deftest is testing]]
            [gitsnitch.metrics.danger :as danger]
            [gitsnitch.fixtures :as fx]))

(deftest danger-stats-matches-default-keywords
  (let [commits [(fx/commit-map {:commit/subject "revert previous change" :commit/author-name "Alice"
                                 :commit/files ["a.clj"] :commit/author-date "2024-01-01T00:00:00Z"})
                 (fx/commit-map {:commit/subject "add feature" :commit/author-name "Bob"})]
        result (danger/danger-stats commits {})]
    (is (= 2 (:total-commits result)))
    (is (= 1 (:danger-commits result)))
    (is (= [["Alice" 1]] (:by-author result)))
    (is (= [["a.clj" 1]] (:by-file result)))
    (is (= [["2024-01" 1]] (:by-month result)))))

(deftest danger-stats-top-limits-by-author-and-by-file
  (let [commits (for [n (range 5)]
                  (fx/commit-map {:commit/subject "hotfix" :commit/author-name (str "author-" n)
                                  :commit/files [(str "file-" n ".clj")]}))
        result (danger/danger-stats commits {:top 2})]
    (is (= 2 (count (:by-author result))))
    (is (= 2 (count (:by-file result))))))

(deftest danger-stats-recent-commits-most-recent-first
  (let [commits [(fx/commit-map {:commit/subject "hack: quick patch" :commit/short "aaa"
                                 :commit/author-date "2024-01-01T00:00:00Z"})
                 (fx/commit-map {:commit/subject "workaround for bug" :commit/short "bbb"
                                 :commit/author-date "2024-01-02T00:00:00Z"})]
        result (danger/danger-stats commits {})]
    (is (= ["bbb" "aaa"] (map :short (:recent-commits result))))))

(deftest danger-stats-no-matches
  (let [commits [(fx/commit-map {:commit/subject "add feature"})]
        result (danger/danger-stats commits {})]
    (is (= 0 (:danger-commits result)))
    (is (empty? (:by-author result)))))
