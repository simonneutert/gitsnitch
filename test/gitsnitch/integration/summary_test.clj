(ns gitsnitch.integration.summary-test
  (:require [clojure.test :refer [deftest is]]
            [cheshire.core :as json]
            [gitsnitch.fixtures :as fx]))

(defn- two-author-repo! []
  (fx/build-repo!
   [{:files {"a.clj" "1"} :message "add a"
     :author {:name "Alice" :email "alice@x.com"} :date "2024-01-01T09:00:00"}
    {:files {"b.clj" "1"} :message "add b"
     :author {:name "Bob" :email "bob@x.com"} :date "2024-01-02T09:00:00"}]))

(deftest summary-table-shows-mailmap-hint-with-multiple-authors-and-no-mailmap
  (let [repo (two-author-repo!)]
    (try
      (let [{:keys [exit out]} (fx/run-gitsnitch repo "summary")]
        (is (zero? exit))
        (is (re-find #"gitsnitch authors --suggest-mailmap" out)))
      (finally (fx/delete-repo! repo)))))

(deftest summary-json-includes-mailmap-hint-with-multiple-authors-and-no-mailmap
  (let [repo (two-author-repo!)]
    (try
      (let [{:keys [out]} (fx/run-gitsnitch repo "summary" "--format" "json")
            data (json/parse-string out true)]
        (is (re-find #"--suggest-mailmap" (:mailmap-hint data))))
      (finally (fx/delete-repo! repo)))))

(deftest summary-hides-mailmap-hint-when-only-one-author
  (let [repo (fx/build-repo!
              [{:files {"a.clj" "1"} :message "a" :author {:name "Solo" :email "s@x.com"} :date "2024-01-01T09:00:00"}
               {:files {"a.clj" "2"} :message "b" :author {:name "Solo" :email "s@x.com"} :date "2024-01-02T09:00:00"}])]
    (try
      (let [{:keys [out]} (fx/run-gitsnitch repo "summary")]
        (is (not (re-find #"suggest-mailmap" out))))
      (finally (fx/delete-repo! repo)))))

(deftest summary-hides-mailmap-hint-when-mailmap-already-present
  (let [repo (fx/build-repo!
              [{:files {"a.clj" "1"} :message "old email"
                :author {:name "Alice" :email "alice@old.com"} :date "2024-01-01T09:00:00"}
               {:files {".mailmap" "Alice <alice@old.com>\n"} :message "add mailmap"
                :author {:name "Alice" :email "alice@old.com"} :date "2024-01-02T09:00:00"}
               {:files {"b.clj" "1"} :message "bob's commit"
                :author {:name "Bob" :email "bob@x.com"} :date "2024-01-03T09:00:00"}])]
    (try
      (let [{:keys [out]} (fx/run-gitsnitch repo "summary")]
        (is (not (re-find #"suggest-mailmap" out))))
      (finally (fx/delete-repo! repo)))))
