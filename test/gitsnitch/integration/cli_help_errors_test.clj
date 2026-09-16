(ns gitsnitch.integration.cli-help-errors-test
  (:require [clojure.test :refer [deftest is testing use-fixtures]]
            [babashka.fs :as fs]
            [gitsnitch.fixtures :as fx]))

(def ^:dynamic *non-repo-dir* nil)

(use-fixtures :each
  (fn [f]
    (binding [*non-repo-dir* (fs/create-temp-dir {:prefix "gitsnitch-test-non-repo-"})]
      (try (f) (finally (fs/delete-tree *non-repo-dir* {:force true}))))))

(deftest help-with-no-args-works-outside-a-repo
  (let [{:keys [exit out]} (fx/run-gitsnitch *non-repo-dir*)]
    (is (zero? exit))
    (is (re-find #"Usage: gitsnitch" out))))

(deftest help-flag-works-outside-a-repo
  (let [{:keys [exit out]} (fx/run-gitsnitch *non-repo-dir* "--help")]
    (is (zero? exit))
    (is (re-find #"Usage: gitsnitch" out))))

(deftest command-help-works-outside-a-repo
  (let [{:keys [exit out]} (fx/run-gitsnitch *non-repo-dir* "churn" "--help")]
    (is (zero? exit))
    (is (re-find #"Usage: gitsnitch churn" out))))

(deftest unknown-command-exits-nonzero-with-error
  (let [{:keys [exit err]} (fx/run-gitsnitch *non-repo-dir* "bogus")]
    (is (not (zero? exit)))
    (is (re-find #"Unknown command: bogus" err))))

(deftest non-repo-directory-exits-nonzero
  (let [{:keys [exit err]} (fx/run-gitsnitch *non-repo-dir* "churn")]
    (is (not (zero? exit)))
    (is (re-find #"not inside a Git repository" err))))
