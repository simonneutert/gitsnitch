(ns gitsnitch.unit.metrics.churn-test
  (:require [clojure.test :refer [deftest is testing]]
            [gitsnitch.metrics.churn :as churn]
            [gitsnitch.fixtures :as fx]))

(deftest file-churn-ranks-by-change-count
  (let [commits [(fx/commit-map {:commit/files ["a.clj"] :commit/author-date "2024-01-01T00:00:00Z"})
                 (fx/commit-map {:commit/files ["a.clj" "b.clj"] :commit/author-date "2024-01-02T00:00:00Z"})]
        result (churn/file-churn commits {})]
    (is (= 2 (:total-commits result)))
    (is (= 2 (:total-files result)))
    (is (= ["a.clj" "b.clj"] (map :path (:rows result))))
    (is (= 2 (:changes (first (:rows result)))))))

(deftest file-churn-exclude-pattern
  (let [commits [(fx/commit-map {:commit/files ["a.clj" "b.lock"]})]
        result (churn/file-churn commits {:exclude ["*.lock"]})]
    (is (= ["a.clj"] (map :path (:rows result))))))

(deftest file-churn-path-filter
  (let [commits [(fx/commit-map {:commit/files ["src/a.clj" "docs/readme.md"]})]
        result (churn/file-churn commits {:path ["src/"]})]
    (is (= ["src/a.clj"] (map :path (:rows result))))))

(deftest file-churn-detailed-includes-insertions-deletions
  (let [commits [(fx/numstat-commit {} [{:path "a.clj" :insertions 5 :deletions 2}])]
        result (churn/file-churn commits {:detailed? true})]
    (is (= 5 (:insertions (first (:rows result)))))
    (is (= 2 (:deletions (first (:rows result)))))))

(deftest dir-churn-aggregates-by-directory
  (let [commits [(fx/commit-map {:commit/files ["src/a.clj" "src/b.clj"]})
                 (fx/commit-map {:commit/files ["docs/readme.md"]})]
        result (churn/dir-churn commits {})]
    (is (= 2 (:total-dirs result)))
    (is (some #(and (= "src/" (:dir %)) (= 2 (:file-count %))) (:rows result)))))

(deftest dir-churn-root-files-use-dot-slash
  (let [commits [(fx/commit-map {:commit/files ["README.md"]})]
        result (churn/dir-churn commits {})]
    (is (= "./" (:dir (first (:rows result)))))))

(deftest accumulate-step-matches-file-churn
  (testing "the single-pass accumulator agrees with file-churn's own reduce"
    (let [commits [(fx/commit-map {:commit/files ["a.clj"]})
                   (fx/commit-map {:commit/files ["a.clj" "b.clj"]})]
          acc (reduce churn/accumulate-step (churn/init-acc) commits)
          via-acc (churn/finalize-churn acc)
          via-direct (churn/file-churn commits {})]
      (is (= (:rows via-acc) (:rows via-direct))))))
