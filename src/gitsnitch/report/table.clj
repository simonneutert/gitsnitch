(ns gitsnitch.report.table
  (:require [clojure.string :as str]))

(defn- pad-right [s width]
  (let [s (str s)]
    (str s (apply str (repeat (max 0 (- width (count s))) " ")))))

(defn- pad-left [s width]
  (let [s (str s)]
    (str (apply str (repeat (max 0 (- width (count s))) " ")) s)))

(defn- format-number [n]
  (if (integer? n)
    (format "%,d" n)
    (str n)))

(defn render-table
  "Render a sequence of maps as a plain-text table.
   columns is a vector of {:key :label :align (:left/:right) :width :format-fn}"
  [columns rows & {:keys [title footer]}]
  (let [sb (StringBuilder.)]
    (when title
      (.append sb (str "\n" title "\n\n")))
    ;; Header
    (let [header-line (str/join "   "
                                (map (fn [{:keys [label width align]}]
                                       (if (= align :right)
                                         (pad-left label width)
                                         (pad-right label width)))
                                     columns))]
      (.append sb (str header-line "\n"))
      (.append sb (str (apply str (repeat (count header-line) "─")) "\n")))
    ;; Rows
    (doseq [row rows]
      (let [line (str/join "   "
                           (map (fn [{:keys [key width align format-fn]}]
                                  (let [val (get row key)
                                        formatted (if format-fn
                                                    (format-fn val)
                                                    (str val))]
                                    (if (= align :right)
                                      (pad-left formatted width)
                                      (pad-right formatted width))))
                                columns))]
        (.append sb (str line "\n"))))
    (when footer
      (.append sb (str "\n" footer "\n")))
    (str sb)))

;; Churn table

(defn churn-table [data {:keys [top detailed?] :or {top 20}}]
  (let [rows (take top (:rows data))
        columns (cond-> [{:key :rank    :label "#"          :align :right :width 3
                          :format-fn str}
                         {:key :changes :label "Changes"    :align :right :width 8
                          :format-fn format-number}
                         {:key :percent :label "%"          :align :right :width 6
                          :format-fn #(format "%.1f%%" %)}
                         {:key :last-changed :label "Last changed" :align :left :width 12
                          :format-fn #(if % (subs (str %) 0 (min 10 (count (str %)))) "—")}]
                  detailed?
                  (into [{:key :insertions :label "+Ins" :align :right :width 6
                          :format-fn format-number}
                         {:key :deletions  :label "-Del" :align :right :width 6
                          :format-fn format-number}])

                  true
                  (conj {:key :path :label "Path" :align :left :width 40}))
        numbered (map-indexed (fn [i row] (assoc row :rank (inc i))) rows)]
    (render-table columns numbered
                  :title (str (if detailed? "Churn" "Frequency")
                              " — top " (count rows)
                              " files (" (:total-commits data) " commits)")
                  :footer (str (format-number (:total-commits data)) " commits analyzed · "
                               (format-number (:total-files data)) " files seen"))))

(defn dir-churn-table [data {:keys [top] :or {top 20}}]
  (let [rows (take top (:rows data))
        columns [{:key :rank      :label "#"          :align :right :width 3
                  :format-fn str}
                 {:key :changes   :label "Changes"    :align :right :width 8
                  :format-fn format-number}
                 {:key :percent   :label "%"          :align :right :width 6
                  :format-fn #(format "%.1f%%" %)}
                 {:key :file-count :label "Files"     :align :right :width 5
                  :format-fn format-number}
                 {:key :last-changed :label "Last changed" :align :left :width 12
                  :format-fn #(if % (subs (str %) 0 (min 10 (count (str %)))) "—")}
                 {:key :dir       :label "Directory"  :align :left :width 40}]
        numbered (map-indexed (fn [i row] (assoc row :rank (inc i))) rows)]
    (render-table columns numbered
                  :title (str "Churn — top " (count rows)
                              " directories (" (:total-commits data) " commits)")
                  :footer (str (format-number (:total-commits data)) " commits analyzed · "
                               (format-number (:total-dirs data)) " directories seen"))))

;; Authors table

(defn authors-table [data {:keys [top] :or {top 20}}]
  (let [rows (take top (:rows data))
        columns [{:key :rank      :label "#"       :align :right :width 3 :format-fn str}
                 {:key :commits   :label "Commits" :align :right :width 8 :format-fn format-number}
                 {:key :percent   :label "%"       :align :right :width 6
                  :format-fn #(format "%.1f%%" %)}
                 {:key :last-seen :label "Last seen" :align :left :width 12
                  :format-fn #(if % (subs (str %) 0 (min 10 (count (str %)))) "—")}
                 {:key :author    :label "Author"  :align :left :width 30}]
        numbered (map-indexed (fn [i row] (assoc row :rank (inc i))) rows)]
    (render-table columns numbered
                  :title (str "Authors — top " (count rows)
                              " (" (:total-commits data) " commits)")
                  :footer (str (format-number (:total-commits data)) " commits · "
                               (format-number (:total-authors data)) " authors"))))

;; Mailmap suggestions

(defn- pluralize [n singular plural]
  (if (= n 1) singular plural))

(defn- commits-phrase [n]
  (str (format-number n) " " (pluralize n "commit" "commits")))

(defn mailmap-suggestions-table
  "Render suggested .mailmap entries for clusters of likely-duplicate
   identities (see gitsnitch.metrics.authors/suggest-mailmap)."
  [clusters]
  (let [sb (StringBuilder.)]
    (.append sb (str "\nSuggested .mailmap entries — " (count clusters)
                     " likely-duplicate " (pluralize (count clusters) "identity" "identities")
                     " found\n"))
    (if (empty? clusters)
      (.append sb "\nNo likely duplicates found.\n")
      (do
        (doseq [{:keys [canonical aliases]} clusters]
          (.append sb (str "\n" (:name canonical) " <" (:email canonical) ">  ("
                           (commits-phrase (:commits canonical)) ")\n"))
          (doseq [{:keys [name email commits last-seen]} aliases]
            (.append sb (str "  ← " name " <" email ">  ("
                             (commits-phrase commits)
                             (when last-seen
                               (str ", last seen " (subs (str last-seen) 0 (min 10 (count (str last-seen)))))) ")\n"))))
        (.append sb "\n--- paste into .mailmap ---\n")
        (doseq [{:keys [mailmap-lines]} clusters
                line mailmap-lines]
          (.append sb (str line "\n")))))
    (str sb)))

;; Activity table

(defn activity-table [data {:keys [trend]}]
  (let [rows (:rows data)
        columns [{:key :period  :label "Period"  :align :left :width 10}
                 {:key :commits :label "Commits" :align :right :width 8 :format-fn format-number}
                 {:key :bar     :label ""        :align :left :width 40}]
        max-commits (apply max 1 (map :commits rows))
        bar-width 30
        with-bars (map (fn [row]
                         (let [w (int (* bar-width (/ (:commits row) max-commits)))]
                           (assoc row :bar (apply str (repeat w "█")))))
                       rows)]
    (render-table columns with-bars
                  :title (str "Activity (" (:total-commits data) " commits)")
                  :footer (when trend
                            (str "Trend: " (name trend))))))

;; Coupling table

(defn coupling-table [data {:keys [top] :or {top 20}}]
  (let [rows (take top (:rows data))
        columns [{:key :rank       :label "#"     :align :right :width 3 :format-fn str}
                 {:key :cochanges  :label "Co-chg" :align :right :width 7 :format-fn format-number}
                 {:key :support    :label "Supp%" :align :right :width 6
                  :format-fn #(format "%.1f%%" %)}
                 {:key :confidence-ab :label "A→B%" :align :right :width 5
                  :format-fn #(format "%.0f%%" %)}
                 {:key :confidence-ba :label "B→A%" :align :right :width 5
                  :format-fn #(format "%.0f%%" %)}
                 {:key :jaccard    :label "Jacc" :align :right :width 5
                  :format-fn #(format "%.2f" %)}
                 {:key :file-a     :label "File A" :align :left :width 30}
                 {:key :file-b     :label "File B" :align :left :width 30}]
        numbered (map-indexed (fn [i row] (assoc row :rank (inc i))) rows)]
    (render-table columns numbered
                  :title (str "Coupling — top " (count rows)
                              " pairs (" (:total-commits data) " commits)")
                  :footer (str (format-number (:total-commits data)) " commits analyzed · "
                               (format-number (:total-pairs data)) " pairs found"
                               (when (pos? (:skipped-commits data))
                                 (str " · " (format-number (:skipped-commits data))
                                      " oversized commits skipped"))))))

;; Bugs table

(defn bugs-table [data {:keys [top] :or {top 20}}]
  (let [rows (take top (:rows data))
        columns [{:key :rank        :label "#"          :align :right :width 3
                  :format-fn str}
                 {:key :bug-changes :label "Bug commits" :align :right :width 11
                  :format-fn format-number}
                 {:key :percent     :label "%"           :align :right :width 6
                  :format-fn #(format "%.1f%%" %)}
                 {:key :last-bug    :label "Last bug"    :align :left :width 12
                  :format-fn #(if % (subs (str %) 0 (min 10 (count (str %)))) "—")}
                 {:key :path        :label "Path"        :align :left :width 40}]
        numbered (map-indexed (fn [i row] (assoc row :rank (inc i))) rows)]
    (render-table columns numbered
                  :title (str "Bug hotspots — top " (count rows)
                              " files (" (:bug-commits data) " bug commits"
                              " / " (:total-commits data) " total)")
                  :footer (str (format-number (:bug-commits data)) " bug-related commits · "
                               (format-number (:total-files data)) " files touched"))))

;; Danger table

(defn danger-table
  "Render the full danger report: top files, top authors,
   firefighting density by month, and recent matching commits."
  [data _opts]
  (let [sb (StringBuilder.)]
    ;; Header
    (.append sb (str "\nDanger signals — " (:danger-commits data)
                     " matching commits / " (:total-commits data) " total\n"))

    ;; Top files
    (when (seq (:by-file data))
      (.append sb "\n")
      (.append sb (render-table
                   [{:key :rank  :label "#"     :align :right :width 3 :format-fn str}
                    {:key :count :label "Hits"  :align :right :width 8 :format-fn format-number}
                    {:key :path  :label "Path"  :align :left :width 50}]
                   (map-indexed (fn [i [path cnt]]
                                  {:rank (inc i) :count cnt :path path})
                                (:by-file data))
                   :title "Top files")))

    ;; Top authors
    (when (seq (:by-author data))
      (.append sb "\n")
      (.append sb (render-table
                   [{:key :rank   :label "#"      :align :right :width 3 :format-fn str}
                    {:key :count  :label "Hits"   :align :right :width 8 :format-fn format-number}
                    {:key :author :label "Author" :align :left :width 30}]
                   (map-indexed (fn [i [author cnt]]
                                  {:rank (inc i) :count cnt :author author})
                                (:by-author data))
                   :title "Top authors")))

    ;; Firefighting density by month
    (when (seq (:by-month data))
      (let [rows      (:by-month data)
            max-count (apply max 1 (map second rows))
            bar-width 25]
        (.append sb "\n")
        (.append sb (render-table
                     [{:key :month   :label "Month"   :align :left :width 8}
                      {:key :count   :label "Danger"  :align :right :width 8 :format-fn format-number}
                      {:key :bar     :label ""         :align :left :width 30}]
                     (map (fn [[month cnt]]
                            (let [w (int (* bar-width (/ cnt max-count)))]
                              {:month month :count cnt
                               :bar (apply str (repeat w "▓"))}))
                          rows)
                     :title "Firefighting density"))))

    ;; Recent commits
    (when (seq (:recent-commits data))
      (.append sb "\n")
      (.append sb (render-table
                   [{:key :short   :label "Commit"  :align :left :width 8}
                    {:key :date    :label "Date"    :align :left :width 12
                     :format-fn #(if % (subs (str %) 0 (min 10 (count (str %)))) "—")}
                    {:key :author  :label "Author"  :align :left :width 20}
                    {:key :subject :label "Subject" :align :left :width 50}]
                   (:recent-commits data)
                   :title "Recent danger commits")))

    (str sb)))
