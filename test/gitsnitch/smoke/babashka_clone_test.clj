(ns gitsnitch.smoke.babashka-clone-test
  "Smoke suite: runs gitsnitch against a real, cloned repo (babashka's own,
   main branch) and asserts STRUCTURE, not values — the clone's content
   isn't ours to control and will drift over time. Skips (prints, doesn't
   fail) when there's no network and no cache yet."
  (:require [clojure.test :refer [deftest is testing use-fixtures]]
            [cheshire.core :as json]
            [gitsnitch.cache :as cache]
            [gitsnitch.fixtures :as fx]))

(def ^:dynamic *repo-dir* nil)

(use-fixtures :once
  (fn [f]
    (binding [*repo-dir* (cache/ensure-clone!)]
      (f))))

(defmacro skippable [& body]
  `(if (nil? *repo-dir*)
     (println "SKIP: babashka clone unavailable (offline, no cache) —" ~(str *ns*))
     (do ~@body)))

(deftest churn-json-has-required-shape-and-is-sorted
  (skippable
   (let [{:keys [exit out]} (fx/run-gitsnitch *repo-dir* "churn" "--format" "json" "--top" "5")
         data (json/parse-string out true)]
     (is (zero? exit))
     (is (<= 0 (count (:rows data)) 5) "--top 5 should now cap json rows too")
     (is (every? #(and (string? (:path %))
                       (integer? (:changes %))
                       (>= (:changes %) 0))
                 (:rows data)))
     (is (apply >= (map :changes (:rows data)))))))

(deftest authors-json-has-required-shape
  (skippable
   (let [{:keys [exit out]} (fx/run-gitsnitch *repo-dir* "authors" "--format" "json")
         data (json/parse-string out true)]
     (is (zero? exit))
     (is (contains? data :total-commits))
     (is (contains? data :total-authors))
     (is (every? #(and (string? (:author %)) (integer? (:commits %))) (:rows data)))
     (is (apply >= (map :commits (:rows data)))))))

(deftest coupling-json-has-required-shape
  (skippable
   (let [{:keys [exit out]} (fx/run-gitsnitch *repo-dir* "coupling" "--format" "json"
                                              "--min-cochanges" "5")
         data (json/parse-string out true)]
     (is (zero? exit))
     (is (every? #(and (string? (:file-a %)) (string? (:file-b %))
                       (>= (:cochanges %) 5))
                 (:rows data))))))

(deftest every-command-format-combo-exits-cleanly
  (skippable
   (doseq [cmd ["summary" "churn" "authors" "activity" "coupling" "bugs" "danger"]
           fmt ["table" "json" "edn"]]
     (testing (str cmd " --format " fmt)
       (let [{:keys [exit out err]} (fx/run-gitsnitch *repo-dir* cmd "--format" fmt "--limit" "500")]
         (is (zero? exit) (str cmd " " fmt " failed: " err))
         (case fmt
           "json" (is (map? (json/parse-string out true)))
           "edn"  (is (map? (clojure.edn/read-string out)))
           (is (string? out))))))))
