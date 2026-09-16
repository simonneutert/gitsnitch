(ns gitsnitch.integration.authors-test
  (:require [clojure.test :refer [deftest is testing use-fixtures]]
            [cheshire.core :as json]
            [gitsnitch.fixtures :as fx]))

(def ^:dynamic *repo* nil)

(use-fixtures :each
  (fn [f]
    (binding [*repo* (fx/build-repo!
                      [{:files {"a.clj" "1"} :message "add a"
                        :author {:name "Alice" :email "alice@x.com"} :date "2024-01-01T09:00:00"}
                       {:files {"a.clj" "2"} :message "tweak a"
                        :author {:name "Alice" :email "alice@x.com"} :date "2024-01-02T09:00:00"}
                       {:files {"b.clj" "1"} :message "add b"
                        :author {:name "Bob" :email "bob@x.com"} :date "2024-01-03T09:00:00"}])]
      (try (f) (finally (fx/delete-repo! *repo*))))))

(deftest authors-json-ranks-by-commits
  (let [{:keys [exit out]} (fx/run-gitsnitch *repo* "authors" "--format" "json")
        data (json/parse-string out true)]
    (is (zero? exit))
    (is (= "Alice" (:author (first (:rows data)))))
    (is (= 2 (:commits (first (:rows data)))))
    (is (contains? data :warnings))))

(deftest authors-bus-factor-warning-fires
  (testing "single author with >60% of commits triggers a severe warning"
    (let [repo (fx/build-repo!
                [{:files {"a.clj" "1"} :message "a" :author {:name "Solo" :email "s@x.com"} :date "2024-01-01T09:00:00"}
                 {:files {"a.clj" "2"} :message "b" :author {:name "Solo" :email "s@x.com"} :date "2024-01-02T09:00:00"}
                 {:files {"a.clj" "3"} :message "c" :author {:name "Other" :email "o@x.com"} :date "2024-01-03T09:00:00"}])]
      (try
        (let [{:keys [out]} (fx/run-gitsnitch repo "authors" "--format" "json")
              data (json/parse-string out true)]
          (is (some #(= "severe" (:level %)) (:warnings data))))
        (finally (fx/delete-repo! repo))))))

(deftest authors-table-shows-warning-line
  (let [{:keys [exit out]} (fx/run-gitsnitch *repo* "authors")]
    (is (zero? exit))
    (is (re-find #"Alice" out))))
