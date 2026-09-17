(ns gitsnitch.cli
  (:require [babashka.cli :as cli]
            [clojure.string :as str]))

(def global-spec
  {:rev     {:desc "Git revision or range"
             :alias :r}
   :since   {:desc "Lower bound on author date (e.g. 3.months.ago, 2026-01-01)"}
   :until   {:desc "Upper bound on author date (e.g. 1.week.ago, 2026-04-01)"}
   :path    {:desc "Limit to files under this path prefix"
             :coerce []
             :alias :p}
   :top     {:desc "Number of results to show"
             :coerce :int
             :default 20}
   :limit   {:desc "Maximum number of commits to process"
             :coerce :int
             :alias :l}
   :format  {:desc "Output format: table, json, edn"
             :default "table"
             :alias :f}
   :no-merges    {:desc "Exclude merge commits"
                  :coerce :boolean
                  :default true}
   :first-parent {:desc "Follow only first parent of merges"
                  :coerce :boolean}
   :help    {:desc "Show help"
             :coerce :boolean
             :alias :h}})

(def churn-spec
  (merge global-spec
         {:by      {:desc "Aggregate by: file, dir"
                    :default "file"}
          :detailed {:desc "Include insertions/deletions"
                     :coerce :boolean}
          :exclude {:desc "Glob pattern to exclude (repeatable)"
                    :coerce []}}))

(def summary-spec
  (merge global-spec
         {:by  {:desc "Bucket activity by: month, week, day"
                :default "month"}
          :top {:desc "Number of results to show"
                :coerce :int
                :default 10}}))

(def authors-spec
  (merge global-spec
         {:suggest-mailmap {:desc "Suggest .mailmap entries for likely-duplicate identities"
                            :coerce :boolean}}))

(def activity-spec
  (merge global-spec
         {:by {:desc "Bucket by: month, week, day"
               :default "month"}}))

(def bugs-spec
  (merge global-spec
         {:grep {:desc "Regex pattern for bug-related keywords"
                 :default "fix|bug|broken|defect|issue|repair|patch"}
          :exclude {:desc "Glob pattern to exclude (repeatable)"
                    :coerce []}}))

(def danger-spec
  (merge global-spec
         {:grep {:desc "Regex pattern for danger keywords"
                 :default "revert|hotfix|rollback|emergency|workaround|hack"}}))

(def coupling-spec
  (merge global-spec
         {:min-cochanges {:desc "Minimum co-change count to include"
                          :coerce :int
                          :default 2}
          :max-files-per-commit {:desc "Skip commits touching more than N files"
                                 :coerce :int
                                 :default 50}
          :exclude {:desc "Glob pattern to exclude (repeatable)"
                    :coerce []}}))

(def command-specs
  {"summary"  summary-spec
   "churn"    churn-spec
   "authors"  authors-spec
   "activity" activity-spec
   "coupling" coupling-spec
   "bugs"     bugs-spec
   "danger"   danger-spec})

(def ^:private command-descriptions
  {"summary"  "Overview: authors, churn, and activity in a single pass"
   "churn"    "Top files by commit frequency"
   "authors"  "Contributor ranking and bus-factor signals"
   "activity" "Commit activity over time"
   "bugs"     "Bug-hotspot files by commit message pattern"
   "danger"   "Firefighting signal in commit messages"
   "coupling" "Files that change together"})

(defn- format-option [[k spec-entry]]
  (let [flag  (str "--" (name k))
        alias (when-let [a (:alias spec-entry)] (str ", -" (name a)))
        left  (str "  " flag (or alias ""))
        desc  (str (:desc spec-entry "")
                   (when-let [d (:default spec-entry)]
                     (str " (default: " d ")")))]
    (format "%-26s%s" left desc)))

(defn format-global-help []
  (str "Usage: gitsnitch <command> [options]\n"
       "\n"
       "Commands:\n"
       (str/join "\n"
                 (map (fn [[cmd desc]] (format "  %-12s%s" cmd desc))
                      (sort-by first command-descriptions)))
       "\n\n"
       "Global options:\n"
       (str/join "\n" (map format-option (sort-by first global-spec)))
       "\n\n"
       "Run 'gitsnitch <command> --help' for command-specific options.\n"))

(defn format-command-help [cmd]
  (let [spec     (get command-specs cmd)
        cmd-only (apply dissoc spec (keys global-spec))
        desc     (get command-descriptions cmd "")]
    (str "Usage: gitsnitch " cmd " [options]\n"
         "\n"
         "  " desc "\n"
         (when (seq cmd-only)
           (str "\n"
                "Command options:\n"
                (str/join "\n" (map format-option (sort-by first cmd-only)))
                "\n"))
         "\nGlobal options:\n"
         (str/join "\n" (map format-option (sort-by first global-spec)))
         "\n")))

(defn parse-args
  "Parse CLI args, returning {:command str :opts map}, {:help :global},
   {:help :command :command str}, or {:error str}."
  [args]
  (let [cmd      (first args)
        rest-args (rest args)]
    (cond
      (nil? cmd)
      {:help :global}

      (contains? #{"help" "--help" "-h"} cmd)
      {:help :global}

      (not (contains? command-specs cmd))
      {:error (str "Unknown command: " cmd ". Available: summary, churn, authors, activity, coupling, bugs, danger")}

      :else
      (let [spec (get command-specs cmd)
            opts (cli/parse-opts rest-args {:spec spec})]
        (if (:help opts)
          {:help :command :command cmd}
          {:command cmd :opts opts})))))
