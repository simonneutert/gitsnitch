(ns gitsnitch.main
  (:require [gitsnitch.cli :as cli]
            [gitsnitch.git :as git]
            [gitsnitch.progress :as progress]
            [gitsnitch.metrics.churn :as churn]
            [gitsnitch.metrics.authors :as authors]
            [gitsnitch.metrics.activity :as activity]
            [gitsnitch.metrics.bugs :as bugs]
            [gitsnitch.metrics.danger :as danger]
            [gitsnitch.metrics.coupling :as coupling]
            [gitsnitch.report.table :as table]
            [gitsnitch.report.json :as json]))

(defn- show-progress?
  "Should we show a progress indicator? Only for human-readable table output."
  [opts]
  (= (or (:format opts) "table") "table"))

(defn- with-progress
  "Wrap a commit stream with progress if appropriate, and return it.
   Returns the (possibly wrapped) commit seq."
  [commits opts]
  (if (show-progress? opts)
    (let [total (try (git/commit-count opts) (catch Exception _ nil))]
      (progress/wrap-progress commits total))
    commits))

(defn- run-churn [{:keys [opts]}]
  (try
    (let [detailed? (:detailed opts)
          commits   (with-progress
                      (if detailed?
                        (git/stream-commits-detailed opts)
                        (git/stream-commits opts))
                      opts)
          by        (or (:by opts) "file")
          data      (if (= by "dir")
                      (churn/dir-churn commits opts)
                      (churn/file-churn commits (assoc opts :detailed? detailed?)))
          fmt       (or (:format opts) "table")]
      (case fmt
        "json" (json/print-json (assoc data :command "churn" :by by))
        "edn"  (prn (assoc data :command :churn :by (keyword by)))
        (if (= by "dir")
          (print (table/dir-churn-table data opts))
          (print (table/churn-table data (assoc opts :detailed? detailed?))))))
    (finally
      (when (show-progress? opts) (progress/finish-progress!)))))

(defn- run-authors [{:keys [opts]}]
  (try
    (let [commits  (with-progress (git/stream-commits opts) opts)
          data     (authors/author-stats commits opts)
          warnings (authors/concentration-warnings (:rows data) (:total-commits data))
          fmt      (or (:format opts) "table")]
      (case fmt
        "json" (json/print-json (assoc data :command "authors" :warnings warnings))
        "edn"  (prn (assoc data :command :authors :warnings warnings))
        (do
          (print (table/authors-table data opts))
          (doseq [w warnings]
            (println (str "⚠ " (:message w)))))))
    (finally
      (when (show-progress? opts) (progress/finish-progress!)))))

(defn- run-activity [{:keys [opts]}]
  (try
    (let [commits (with-progress (git/stream-commits opts) opts)
          data    (activity/activity-buckets commits opts)
          trend   (activity/trend-summary (:rows data))
          fmt     (or (:format opts) "table")]
      (case fmt
        "json" (json/print-json (assoc data :command "activity" :trend trend))
        "edn"  (prn (assoc data :command :activity :trend trend))
        (print (table/activity-table data {:trend trend}))))
    (finally
      (when (show-progress? opts) (progress/finish-progress!)))))

(defn- run-summary [{:keys [opts]}]
  ;; Single-pass: compute churn + authors + activity in one reduce over the stream
  (try
    (let [commits   (with-progress (git/stream-commits opts) opts)
          by        (or (:by opts) "month")
          top-n     (or (:top opts) 10)
          result    (reduce
                     (fn [acc commit]
                       (-> acc
                           (update :churn-acc   churn/accumulate-step commit)
                           (update :author-acc  authors/accumulate-step commit)
                           (update :activity-acc activity/accumulate-step commit)
                           (update :total inc)
                           (cond->
                            (:commit/merge? commit) (update :merges inc))))
                     {:churn-acc    (churn/init-acc)
                      :author-acc   (authors/init-acc)
                      :activity-acc (activity/init-acc by)
                      :total        0
                      :merges       0}
                     commits)
          total       (:total result)
          merges      (:merges result)
          churn-data  (churn/finalize-churn (:churn-acc result))
          author-data (authors/finalize-authors (:author-acc result))
          act-data    (activity/finalize-activity (:activity-acc result))
          warnings    (authors/concentration-warnings (:rows author-data)
                                                      (:total-commits author-data))
          merge-ratio (if (pos? total)
                        (Double/parseDouble (format "%.1f" (* 100.0 (/ merges total))))
                        0.0)
          out         {:command         "summary"
                       :total-commits   total
                       :merge-commits   merges
                       :merge-ratio     merge-ratio
                       :total-authors   (:total-authors author-data)
                       :top-churn       (take top-n (:rows churn-data))
                       :top-authors     (take top-n (:rows author-data))
                       :recent-activity (take-last 12 (:rows act-data))
                       :warnings        warnings}
          fmt         (or (:format opts) "table")]
      (case fmt
        "json" (json/print-json out)
        "edn"  (prn (update out :command keyword))
        ;; default: table
        (do
          (println)
          (println (str "gitsnitch summary — " total " commits, "
                        (:total-authors author-data) " authors, "
                        merges " merges ("
                        (if (pos? total)
                          (format "%.0f%%" (* 100.0 (/ merges total)))
                          "0%")
                        ")"))
          (println)
          (print (table/activity-table act-data {:trend (activity/trend-summary (:rows act-data))}))
          (doseq [w warnings]
            (println (str "\n⚠ " (:message w))))
          (println)
          (print (table/churn-table (assoc churn-data :total-commits total)
                                    {:top top-n}))
          (println)
          (print (table/authors-table author-data {:top top-n}))
          (println))))
    (finally
      (when (show-progress? opts) (progress/finish-progress!)))))

(defn- run-bugs [{:keys [opts]}]
  (try
    (let [commits (with-progress (git/stream-commits opts) opts)
          data    (bugs/bug-hotspots commits opts)
          fmt     (or (:format opts) "table")]
      (case fmt
        "json" (json/print-json (assoc data :command "bugs"))
        "edn"  (prn (assoc data :command :bugs))
        (print (table/bugs-table data opts))))
    (finally
      (when (show-progress? opts) (progress/finish-progress!)))))

(defn- run-danger [{:keys [opts]}]
  (try
    (let [commits (with-progress (git/stream-commits opts) opts)
          data    (danger/danger-stats commits opts)
          fmt     (or (:format opts) "table")]
      (case fmt
        "json" (json/print-json (assoc data :command "danger"))
        "edn"  (prn (assoc data :command :danger))
        (print (table/danger-table data opts))))
    (finally
      (when (show-progress? opts) (progress/finish-progress!)))))

(defn- run-coupling [{:keys [opts]}]
  (try
    (let [commits (with-progress (git/stream-commits opts) opts)
          data    (coupling/coupling-stats commits opts)
          fmt     (or (:format opts) "table")]
      (case fmt
        "json" (json/print-json (assoc data :command "coupling"))
        "edn"  (prn (assoc data :command :coupling))
        (print (table/coupling-table data opts))))
    (finally
      (when (show-progress? opts) (progress/finish-progress!)))))

(defn -main [& args]
  (let [parsed (cli/parse-args args)]
    (cond
      (= (:help parsed) :global)
      (do (print (cli/format-global-help))
          (flush)
          (System/exit 0))

      (= (:help parsed) :command)
      (do (print (cli/format-command-help (:command parsed)))
          (flush)
          (System/exit 0))

      (:error parsed)
      (do (binding [*out* *err*]
            (println (str "Error: " (:error parsed)))
            (print (cli/format-global-help))
            (flush))
          (System/exit 1))

      :else
      (do
        (when-not (git/repo-root)
          (binding [*out* *err*]
            (println "Error: not inside a Git repository"))
          (System/exit 1))
        (case (:command parsed)
          "summary"  (run-summary parsed)
          "churn"    (run-churn parsed)
          "authors"  (run-authors parsed)
          "activity" (run-activity parsed)
          "bugs"     (run-bugs parsed)
          "danger"   (run-danger parsed)
          "coupling" (run-coupling parsed))))))