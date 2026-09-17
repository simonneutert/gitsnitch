(ns gitsnitch.unit.report.table-test
  (:require [clojure.test :refer [deftest is testing]]
            [gitsnitch.report.table :as table]))

(deftest render-table-basic-layout
  (let [columns [{:key :name :label "Name" :align :left :width 10}
                 {:key :count :label "Count" :align :right :width 5 :format-fn str}]
        rows [{:name "a.clj" :count 3}]
        out (table/render-table columns rows)]
    (is (re-find #"Name" out))
    (is (re-find #"Count" out))
    (is (re-find #"a\.clj" out))))

(deftest render-table-title-and-footer
  (let [out (table/render-table [{:key :x :label "X" :align :left :width 3}] []
                                :title "My Title" :footer "My Footer")]
    (is (re-find #"My Title" out))
    (is (re-find #"My Footer" out))))

(deftest churn-table-respects-top
  (let [data {:total-commits 3 :total-files 3
              :rows [{:path "a.clj" :changes 3 :percent 50.0 :last-changed "2024-01-01"}
                     {:path "b.clj" :changes 2 :percent 33.0 :last-changed "2024-01-02"}
                     {:path "c.clj" :changes 1 :percent 17.0 :last-changed "2024-01-03"}]}
        out (table/churn-table data {:top 1})]
    (is (re-find #"a\.clj" out))
    (is (not (re-find #"b\.clj" out)))))

(deftest authors-table-renders-author-names
  (let [data {:total-commits 2 :total-authors 2
              :rows [{:author "Alice" :commits 1 :percent 50.0 :last-seen "2024-01-01"}
                     {:author "Bob" :commits 1 :percent 50.0 :last-seen "2024-01-02"}]}
        out (table/authors-table data {})]
    (is (re-find #"Alice" out))
    (is (re-find #"Bob" out))))

(deftest activity-table-includes-bar-chart
  (let [data {:total-commits 2 :rows [{:period "2024-01" :commits 5}
                                      {:period "2024-02" :commits 1}]}
        out (table/activity-table data {})]
    (is (re-find #"2024-01" out))
    (is (re-find #"█" out))))

(deftest mailmap-suggestions-table-renders-cluster-and-paste-block
  (let [clusters [{:canonical {:name "Simon Neutert" :email "simonneutert@users.noreply.github.com" :commits 84}
                   :aliases [{:name "Simon" :email "simon.neutert@gmail.com" :commits 30 :last-seen "2019-01-01"}]
                   :mailmap-lines ["Simon Neutert <simonneutert@users.noreply.github.com> Simon <simon.neutert@gmail.com>"]}]
        out (table/mailmap-suggestions-table clusters)]
    (is (re-find #"Simon Neutert" out))
    (is (re-find #"simon\.neutert@gmail\.com" out))
    (is (re-find #"paste into \.mailmap" out))))

(deftest mailmap-suggestions-table-pluralizes-commit-counts
  (let [clusters [{:canonical {:name "Solo" :email "solo@x.com" :commits 1}
                   :aliases [{:name "Solo" :email "solo@old.com" :commits 1 :last-seen "2019-01-01"}]
                   :mailmap-lines ["Solo <solo@x.com> <solo@old.com>"]}]
        out (table/mailmap-suggestions-table clusters)]
    (is (re-find #"1 likely-duplicate identity found" out))
    (is (re-find #"\(1 commit\)" out))
    (is (re-find #"\(1 commit,\s" out))
    (is (not (re-find #"1 commits" out)))))

(deftest mailmap-suggestions-table-handles-no-clusters
  (let [out (table/mailmap-suggestions-table [])]
    (is (re-find #"No likely duplicates found" out))))

(deftest danger-table-handles-empty-sections
  (testing "no crash when all sections are empty"
    (let [out (table/danger-table {:total-commits 0 :danger-commits 0
                                   :by-file [] :by-author [] :by-month []
                                   :recent-commits []}
                                  {})]
      (is (string? out)))))
