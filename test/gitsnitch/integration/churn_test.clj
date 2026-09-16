(ns gitsnitch.integration.churn-test
  (:require [clojure.test :refer [deftest is testing use-fixtures]]
            [cheshire.core :as json]
            [gitsnitch.fixtures :as fx]))

(def ^:dynamic *repo* nil)

(use-fixtures :each
  (fn [f]
    (binding [*repo* (fx/build-repo!
                      [{:files {"a.clj" "1"} :message "add a" :date "2024-01-01T09:00:00"}
                       {:files {"b.clj" "1"} :message "add b" :date "2024-01-02T09:00:00"}
                       {:files {"a.clj" "2"} :message "tweak a" :date "2024-01-03T09:00:00"}])]
      (try (f) (finally (fx/delete-repo! *repo*))))))

(deftest churn-json-sorted-descending-by-changes
  (let [{:keys [exit out]} (fx/run-gitsnitch *repo* "churn" "--format" "json")
        data (json/parse-string out true)]
    (is (zero? exit))
    (is (= "a.clj" (:path (first (:rows data)))))
    (is (= 2 (:changes (first (:rows data)))))
    (is (apply >= (map :changes (:rows data))))))

(deftest churn-detailed-adds-insertions-and-deletions
  (let [{:keys [exit out]} (fx/run-gitsnitch *repo* "churn" "--format" "json" "--detailed")
        data (json/parse-string out true)]
    (is (zero? exit))
    (is (every? #(and (contains? % :insertions) (contains? % :deletions)) (:rows data)))))

(deftest churn-by-dir-aggregates-directories
  (let [{:keys [exit out]} (fx/run-gitsnitch *repo* "churn" "--by" "dir" "--format" "json")
        data (json/parse-string out true)]
    (is (zero? exit))
    (is (every? #(contains? % :dir) (:rows data)))))

(deftest churn-edn-format-parses
  (let [{:keys [exit out]} (fx/run-gitsnitch *repo* "churn" "--format" "edn")]
    (is (zero? exit))
    (let [data (clojure.edn/read-string out)]
      (is (= :churn (:command data)))
      (is (vector? (:rows data))))))

(deftest churn-table-format-does-not-crash
  (let [{:keys [exit out]} (fx/run-gitsnitch *repo* "churn")]
    (is (zero? exit))
    (is (re-find #"a\.clj" out))))
