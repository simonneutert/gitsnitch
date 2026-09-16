(ns gitsnitch.unit.cli-test
  (:require [clojure.test :refer [deftest is testing]]
            [gitsnitch.cli :as cli]))

(deftest parse-args-no-args-is-global-help
  (is (= {:help :global} (cli/parse-args []))))

(deftest parse-args-help-aliases
  (is (= {:help :global} (cli/parse-args ["help"])))
  (is (= {:help :global} (cli/parse-args ["--help"])))
  (is (= {:help :global} (cli/parse-args ["-h"]))))

(deftest parse-args-unknown-command-is-error
  (let [result (cli/parse-args ["bogus"])]
    (is (contains? result :error))
    (is (re-find #"Unknown command: bogus" (:error result)))))

(deftest parse-args-command-help
  (is (= {:help :command :command "churn"} (cli/parse-args ["churn" "--help"]))))

(deftest parse-args-command-with-opts
  (let [result (cli/parse-args ["churn" "--top" "5" "--format" "json"])]
    (is (= "churn" (:command result)))
    (is (= 5 (:top (:opts result))))
    (is (= "json" (:format (:opts result))))))

(deftest parse-args-defaults-applied
  (let [result (cli/parse-args ["churn"])]
    (is (= 20 (:top (:opts result))))
    (is (= "table" (:format (:opts result))))
    (is (true? (:no-merges (:opts result))))))

(deftest format-global-help-lists-all-commands
  (let [help (cli/format-global-help)]
    (testing "every command name appears"
      (doseq [cmd ["summary" "churn" "authors" "activity" "coupling" "bugs" "danger"]]
        (is (re-find (re-pattern cmd) help))))))

(deftest format-command-help-includes-command-specific-options
  (let [help (cli/format-command-help "churn")]
    (is (re-find #"--by" help))
    (is (re-find #"--detailed" help))))
