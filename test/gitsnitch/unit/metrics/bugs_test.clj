(ns gitsnitch.unit.metrics.bugs-test
  (:require [clojure.test :refer [deftest is testing]]
            [gitsnitch.metrics.bugs :as bugs]
            [gitsnitch.fixtures :as fx]))

(deftest bug-hotspots-matches-default-keywords
  (let [commits [(fx/commit-map {:commit/subject "fix: null pointer" :commit/files ["a.clj"]})
                 (fx/commit-map {:commit/subject "add feature" :commit/files ["b.clj"]})]
        result (bugs/bug-hotspots commits {})]
    (is (= 2 (:total-commits result)))
    (is (= 1 (:bug-commits result)))
    (is (= ["a.clj"] (map :path (:rows result))))))

(deftest bug-hotspots-case-insensitive
  (let [commits [(fx/commit-map {:commit/subject "FIX: Broken thing" :commit/files ["a.clj"]})]
        result (bugs/bug-hotspots commits {})]
    (is (= 1 (:bug-commits result)))))

(deftest bug-hotspots-custom-grep
  (let [commits [(fx/commit-map {:commit/subject "typo in comment" :commit/files ["a.clj"]})]
        result (bugs/bug-hotspots commits {:grep "typo"})]
    (is (= 1 (:bug-commits result)))))

(deftest bug-hotspots-no-matches
  (let [commits [(fx/commit-map {:commit/subject "add feature"})]
        result (bugs/bug-hotspots commits {})]
    (is (= 0 (:bug-commits result)))
    (is (empty? (:rows result)))))

(deftest bug-hotspots-exclude-filter
  (let [commits [(fx/commit-map {:commit/subject "fix bug" :commit/files ["a.clj" "b.lock"]})]
        result (bugs/bug-hotspots commits {:exclude ["*.lock"]})]
    (is (= ["a.clj"] (map :path (:rows result))))))
