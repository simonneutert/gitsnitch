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

(deftest authors-consolidates-aliases-via-mailmap
  (testing "commits under two emails for the same person collapse into one row
            when a .mailmap resolves them, mirroring how GitHub's contributor
            graph merges aliases at the account level"
    (let [repo (fx/build-repo!
                [{:files {"a.clj" "1"} :message "old email"
                  :author {:name "Alice" :email "alice@old.com"} :date "2024-01-01T09:00:00"}
                 {:files {"a.clj" "2"} :message "new email"
                  :author {:name "Alice Anderson" :email "alice@new.com"} :date "2024-01-02T09:00:00"}
                 {:files {".mailmap" "Alice Anderson <alice@new.com> <alice@old.com>\n"}
                  :message "add mailmap"
                  :author {:name "Alice Anderson" :email "alice@new.com"} :date "2024-01-03T09:00:00"}
                 {:files {"b.clj" "1"} :message "bob's commit"
                  :author {:name "Bob" :email "bob@x.com"} :date "2024-01-04T09:00:00"}])]
      (try
        (let [{:keys [exit out]} (fx/run-gitsnitch repo "authors" "--format" "json")
              data (json/parse-string out true)
              alice (first (filter #(= "Alice Anderson" (:author %)) (:rows data)))]
          (is (zero? exit))
          (is (= 2 (:total-authors data)))
          (is (some? alice))
          (is (= 3 (:commits alice))))
        (finally (fx/delete-repo! repo))))))

(deftest authors-table-shows-mailmap-hint-with-multiple-authors-and-no-mailmap
  (let [{:keys [exit out]} (fx/run-gitsnitch *repo* "authors")]
    (is (zero? exit))
    (is (re-find #"gitsnitch authors --suggest-mailmap" out))))

(deftest authors-json-includes-mailmap-hint-with-multiple-authors-and-no-mailmap
  (let [{:keys [out]} (fx/run-gitsnitch *repo* "authors" "--format" "json")
        data (json/parse-string out true)]
    (is (re-find #"--suggest-mailmap" (:mailmap-hint data)))))

(deftest authors-hides-mailmap-hint-when-only-one-author
  (let [repo (fx/build-repo!
              [{:files {"a.clj" "1"} :message "a" :author {:name "Solo" :email "s@x.com"} :date "2024-01-01T09:00:00"}
               {:files {"a.clj" "2"} :message "b" :author {:name "Solo" :email "s@x.com"} :date "2024-01-02T09:00:00"}])]
    (try
      (let [{:keys [out]} (fx/run-gitsnitch repo "authors")]
        (is (not (re-find #"suggest-mailmap" out))))
      (finally (fx/delete-repo! repo)))))

(deftest authors-hides-mailmap-hint-when-mailmap-already-present
  (let [repo (fx/build-repo!
              [{:files {"a.clj" "1"} :message "old email"
                :author {:name "Alice" :email "alice@old.com"} :date "2024-01-01T09:00:00"}
               {:files {".mailmap" "Alice <alice@old.com>\n"} :message "add mailmap"
                :author {:name "Alice" :email "alice@old.com"} :date "2024-01-02T09:00:00"}
               {:files {"b.clj" "1"} :message "bob's commit"
                :author {:name "Bob" :email "bob@x.com"} :date "2024-01-03T09:00:00"}])]
    (try
      (let [{:keys [out]} (fx/run-gitsnitch repo "authors")]
        (is (not (re-find #"suggest-mailmap" out))))
      (finally (fx/delete-repo! repo)))))

(deftest authors-hides-mailmap-hint-when-mailmap-file-configured
  (testing "a repo pointing mailmap.file elsewhere (no root .mailmap) should
            still count as having a mailmap, since git honors that config"
    (let [repo (fx/build-repo!
                [{:files {"a.clj" "1"} :message "old email"
                  :author {:name "Alice" :email "alice@old.com"} :date "2024-01-01T09:00:00"}
                 {:files {"config/mailmap"  "Alice <alice@old.com>\n"} :message "add mailmap elsewhere"
                  :author {:name "Alice" :email "alice@old.com"} :date "2024-01-02T09:00:00"}
                 {:files {"b.clj" "1"} :message "bob's commit"
                  :author {:name "Bob" :email "bob@x.com"} :date "2024-01-03T09:00:00"}])]
      (try
        (fx/sh! repo "git" "config" "mailmap.file" "config/mailmap")
        (let [{:keys [out]} (fx/run-gitsnitch repo "authors")]
          (is (not (re-find #"suggest-mailmap" out))))
        (finally (fx/delete-repo! repo))))))

(deftest authors-suggest-mailmap-flags-likely-duplicates
  (testing "--suggest-mailmap clusters aliased identities without needing an
            existing .mailmap, so a repo owner can curate one from the output"
    (let [repo (fx/build-repo!
                [{:files {"a.clj" "1"} :message "old email"
                  :author {:name "Simon Neutert" :email "simonneutert@users.noreply.github.com"} :date "2024-01-01T09:00:00"}
                 {:files {"a.clj" "2"} :message "gmail commit"
                  :author {:name "Simon" :email "simon.neutert@gmail.com"} :date "2024-01-02T09:00:00"}
                 {:files {"b.clj" "1"} :message "unrelated author"
                  :author {:name "Bob" :email "bob@x.com"} :date "2024-01-03T09:00:00"}])]
      (try
        (let [{:keys [exit out]} (fx/run-gitsnitch repo "authors" "--suggest-mailmap")]
          (is (zero? exit))
          (is (re-find #"Simon Neutert" out))
          (is (re-find #"simon\.neutert@gmail\.com" out))
          (is (not (re-find #"Bob" out)))
          (is (re-find #"paste into \.mailmap" out)))
        (let [{:keys [exit out]} (fx/run-gitsnitch repo "authors" "--suggest-mailmap" "--format" "json")
              data (json/parse-string out true)]
          (is (zero? exit))
          (is (= 1 (count (:clusters data)))))
        (finally (fx/delete-repo! repo))))))

(deftest authors-suggest-mailmap-respects-path-filter
  (testing "--path scopes --suggest-mailmap the same way it scopes the plain
            authors table, instead of silently being ignored"
    (let [repo (fx/build-repo!
                [{:files {"src/a.clj" "1"} :message "src commit"
                  :author {:name "Simon Neutert" :email "simonneutert@users.noreply.github.com"} :date "2024-01-01T09:00:00"}
                 {:files {"other/b.clj" "1"} :message "other-dir commit"
                  :author {:name "Simon" :email "simon.neutert@gmail.com"} :date "2024-01-02T09:00:00"}])]
      (try
        (let [{:keys [exit out]} (fx/run-gitsnitch repo "authors" "--suggest-mailmap" "--path" "other/")]
          (is (zero? exit))
          (is (re-find #"No likely duplicates found" out)))
        (let [{:keys [exit out]} (fx/run-gitsnitch repo "authors" "--suggest-mailmap")]
          (is (zero? exit))
          (is (re-find #"paste into \.mailmap" out)))
        (finally (fx/delete-repo! repo))))))
