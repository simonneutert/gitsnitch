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
  (testing "glob excludes match across directory boundaries with **, not a bare *"
    (let [{:keys [exit out]} (fx/run-gitsnitch *repo* "churn" "--format" "json" "--exclude" "**.md")
          data (json/parse-string out true)]
      (is (zero? exit))
      (is (not-any? #(= "docs/readme.md" (:path %)) (:rows data))))))

(deftest top-flag-limits-table-rows
  (testing "--top limits the rendered table for churn"
    (let [{:keys [exit out]} (fx/run-gitsnitch *repo* "churn" "--top" "1")]
      (is (zero? exit))
      (is (re-find #"src/a\.clj" out))
      (is (not (re-find #"src/b\.clj" out))))))

(deftest top-flag-does-not-limit-churn-json
  (testing "gitsnitch's current behavior: --top only affects table rendering for churn/authors/bugs/coupling — json/edn return every row"
    (let [{:keys [exit out]} (fx/run-gitsnitch *repo* "churn" "--format" "json" "--top" "1")
          data (json/parse-string out true)]
      (is (zero? exit))
      (is (= 3 (count (:rows data)))))))

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
