(ns gitsnitch.integration.filters-flags-test
  (:require [clojure.test :refer [deftest is testing use-fixtures]]
            [cheshire.core :as json]
            [gitsnitch.fixtures :as fx]))

(def ^:dynamic *repo* nil)

(use-fixtures :each
  (fn [f]
    (binding [*repo* (fx/build-repo!
                      [{:files {"src/a.clj" "1"} :message "add a" :date "2024-01-01T09:00:00"}
                       {:files {"src/b.clj" "1"} :message "add b" :date "2024-01-02T09:00:00"}
                       {:files {"docs/readme.md" "1"} :message "add docs" :date "2024-01-03T09:00:00"}
                       {:files {"src/a.clj" "2"} :message "tweak a" :date "2024-01-04T09:00:00"}])]
      (try (f) (finally (fx/delete-repo! *repo*))))))

(deftest path-flag-restricts-to-prefix
  (let [{:keys [exit out]} (fx/run-gitsnitch *repo* "churn" "--format" "json" "--path" "src/")
        data (json/parse-string out true)]
    (is (zero? exit))
    (is (every? #(re-find #"^src/" (:path %)) (:rows data)))
    (is (not-any? #(= "docs/readme.md" (:path %)) (:rows data)))))

(deftest exclude-flag-removes-matching-files
  (testing "a slash-free glob excludes matching basenames anywhere in the tree, gitignore-style"
    (let [{:keys [exit out]} (fx/run-gitsnitch *repo* "churn" "--format" "json" "--exclude" "*.md")
          data (json/parse-string out true)]
      (is (zero? exit))
      (is (not-any? #(= "docs/readme.md" (:path %)) (:rows data))))))

(deftest exclude-flag-scoped-pattern-only-matches-that-path
  (testing "a pattern containing / stays scoped to the full relative path"
    (let [{:keys [exit out]} (fx/run-gitsnitch *repo* "churn" "--format" "json" "--exclude" "docs/*")
          data (json/parse-string out true)]
      (is (zero? exit))
      (is (not-any? #(= "docs/readme.md" (:path %)) (:rows data)))
      (is (some #(= "src/a.clj" (:path %)) (:rows data))))))

(deftest top-flag-limits-table-rows
  (testing "--top limits the rendered table for churn"
    (let [{:keys [exit out]} (fx/run-gitsnitch *repo* "churn" "--top" "1")]
      (is (zero? exit))
      (is (re-find #"src/a\.clj" out))
      (is (not (re-find #"src/b\.clj" out))))))

(deftest top-flag-limits-json-and-edn-too
  (testing "--top is honored consistently across every output format for churn"
    (doseq [fmt ["json" "edn"]]
      (testing fmt
        (let [{:keys [exit out]} (fx/run-gitsnitch *repo* "churn" "--format" fmt "--top" "1")
              data (if (= fmt "json") (json/parse-string out true) (clojure.edn/read-string out))]
          (is (zero? exit))
          (is (= 1 (count (:rows data)))))))))

(deftest top-flag-limits-json-for-authors-bugs-and-coupling
  (testing "--top honored consistently (not just for churn) now that run-report caps :rows uniformly"
    (let [repo (fx/build-repo!
                [{:files {"a.clj" "1" "b.clj" "1"} :message "fix: initial a+b"
                  :author {:name "Alice" :email "alice@x.com"} :date "2024-01-01T09:00:00"}
                 {:files {"a.clj" "2" "b.clj" "2"} :message "fix: bug in a+b"
                  :author {:name "Alice" :email "alice@x.com"} :date "2024-01-02T09:00:00"}
                 {:files {"c.clj" "1" "d.clj" "1"} :message "add c+d"
                  :author {:name "Bob" :email "bob@x.com"} :date "2024-01-03T09:00:00"}])]
      (try
        (testing "authors"
          (let [{:keys [exit out]} (fx/run-gitsnitch repo "authors" "--format" "json" "--top" "1")
                data (json/parse-string out true)]
            (is (zero? exit))
            (is (= 1 (count (:rows data))))
            (is (= "Alice" (:author (first (:rows data)))))))
        (testing "bugs"
          (let [{:keys [exit out]} (fx/run-gitsnitch repo "bugs" "--format" "json" "--top" "1")
                data (json/parse-string out true)]
            (is (zero? exit))
            (is (= 1 (count (:rows data))))))
        (testing "coupling"
          (let [{:keys [exit out]} (fx/run-gitsnitch repo "coupling" "--format" "json"
                                                     "--min-cochanges" "1" "--top" "1")
                data (json/parse-string out true)]
            (is (zero? exit))
            (is (= 1 (count (:rows data))))
            (is (= "a.clj" (:file-a (first (:rows data)))))))
        (finally (fx/delete-repo! repo))))))

(deftest all-formats-parse-for-every-command
  (doseq [cmd ["summary" "churn" "authors" "activity" "coupling" "bugs" "danger"]
          fmt ["table" "json" "edn"]]
    (testing (str cmd " --format " fmt)
      (let [{:keys [exit out]} (fx/run-gitsnitch *repo* cmd "--format" fmt)]
        (is (zero? exit) (str cmd " " fmt " exited non-zero"))
        (case fmt
          "json" (is (map? (json/parse-string out true)))
          "edn"  (is (map? (clojure.edn/read-string out)))
          (is (string? out)))))))
