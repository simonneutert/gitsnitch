(ns gitsnitch.integration.coupling-test
  (:require [clojure.test :refer [deftest is testing use-fixtures]]
            [cheshire.core :as json]
            [gitsnitch.fixtures :as fx]))

(def ^:dynamic *repo* nil)

(use-fixtures :each
  (fn [f]
    (binding [*repo* (fx/build-repo!
                      [{:files {"a.clj" "1" "b.clj" "1"} :message "add a+b" :date "2024-01-01T09:00:00"}
                       {:files {"a.clj" "2" "b.clj" "2"} :message "tweak a+b" :date "2024-01-02T09:00:00"}
                       {:files {"c.clj" "1"} :message "add c alone" :date "2024-01-03T09:00:00"}])]
      (try (f) (finally (fx/delete-repo! *repo*))))))

(deftest coupling-json-finds-cochanging-pair
  (let [{:keys [exit out]} (fx/run-gitsnitch *repo* "coupling" "--format" "json" "--min-cochanges" "2")
        data (json/parse-string out true)]
    (is (zero? exit))
    (is (= 1 (count (:rows data))))
    (is (= "a.clj" (:file-a (first (:rows data)))))
    (is (= "b.clj" (:file-b (first (:rows data)))))
    (is (= 2 (:cochanges (first (:rows data)))))))

(deftest coupling-min-cochanges-filters-out-low-counts
  (let [{:keys [exit out]} (fx/run-gitsnitch *repo* "coupling" "--format" "json" "--min-cochanges" "3")
        data (json/parse-string out true)]
    (is (zero? exit))
    (is (empty? (:rows data)))))

(deftest coupling-table-format-does-not-crash
  (let [{:keys [exit out]} (fx/run-gitsnitch *repo* "coupling" "--min-cochanges" "2")]
    (is (zero? exit))
    (is (re-find #"a\.clj" out))))
