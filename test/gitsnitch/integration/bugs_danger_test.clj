(ns gitsnitch.integration.bugs-danger-test
  (:require [clojure.test :refer [deftest is testing use-fixtures]]
            [cheshire.core :as json]
            [gitsnitch.fixtures :as fx]))

(def ^:dynamic *repo* nil)

(use-fixtures :each
  (fn [f]
    (binding [*repo* (fx/build-repo!
                      [{:files {"a.clj" "1"} :message "fix: null pointer" :date "2024-01-01T09:00:00"}
                       {:files {"b.clj" "1"} :message "add feature" :date "2024-01-02T09:00:00"}
                       {:files {"c.clj" "1"} :message "revert previous change" :date "2024-01-03T09:00:00"}])]
      (try (f) (finally (fx/delete-repo! *repo*))))))

(deftest bugs-json-finds-fix-commits
  (let [{:keys [exit out]} (fx/run-gitsnitch *repo* "bugs" "--format" "json")
        data (json/parse-string out true)]
    (is (zero? exit))
    (is (= 1 (:bug-commits data)))
    (is (= "a.clj" (:path (first (:rows data)))))))

(deftest bugs-custom-grep
  (let [{:keys [exit out]} (fx/run-gitsnitch *repo* "bugs" "--format" "json" "--grep" "revert")
        data (json/parse-string out true)]
    (is (zero? exit))
    (is (= 1 (:bug-commits data)))
    (is (= "c.clj" (:path (first (:rows data)))))))

(deftest danger-json-finds-revert-commits
  (let [{:keys [exit out]} (fx/run-gitsnitch *repo* "danger" "--format" "json")
        data (json/parse-string out true)]
    (is (zero? exit))
    (is (= 1 (:danger-commits data)))
    (is (= 1 (count (:by-file data))))))

(deftest bugs-table-format-does-not-crash
  (let [{:keys [exit out]} (fx/run-gitsnitch *repo* "bugs")]
    (is (zero? exit))
    (is (re-find #"a\.clj" out))))

(deftest danger-table-format-does-not-crash
  (let [{:keys [exit out]} (fx/run-gitsnitch *repo* "danger")]
    (is (zero? exit))))
