(ns gitsnitch.integration.activity-test
  (:require [clojure.test :refer [deftest is testing use-fixtures]]
            [cheshire.core :as json]
            [gitsnitch.fixtures :as fx]))

(def ^:dynamic *repo* nil)

(use-fixtures :each
  (fn [f]
    (binding [*repo* (fx/build-repo!
                      [{:files {"a.clj" "1"} :message "a" :date "2024-01-05T09:00:00"}
                       {:files {"a.clj" "2"} :message "b" :date "2024-01-20T09:00:00"}
                       {:files {"a.clj" "3"} :message "c" :date "2024-02-10T09:00:00"}])]
      (try (f) (finally (fx/delete-repo! *repo*))))))

(deftest activity-by-month-buckets-correctly
  (let [{:keys [exit out]} (fx/run-gitsnitch *repo* "activity" "--by" "month" "--format" "json")
        data (json/parse-string out true)]
    (is (zero? exit))
    (is (= [{:period "2024-01" :commits 2} {:period "2024-02" :commits 1}]
           (map #(update % :period identity) (:rows data))))))

(deftest activity-by-day-and-week-do-not-crash
  (doseq [by ["day" "week"]]
    (testing (str "--by " by)
      (let [{:keys [exit]} (fx/run-gitsnitch *repo* "activity" "--by" by "--format" "json")]
        (is (zero? exit))))))
