(ns gitsnitch.metrics.authors
  (:require [clojure.string :as str]
            [gitsnitch.filters :as filters]
            [gitsnitch.util :as util]))

;; ---------------------------------------------------------------------------
;; Accumulator API for single-pass summary
;; ---------------------------------------------------------------------------

(defn init-acc
  "Return initial accumulator state for author-stats."
  []
  {:total 0 :authors {}})

(defn- step-author
  "Update author accumulator state with one commit.
   Keys by author-email; tracks the most-recently-seen author-name."
  [acc commit]
  (let [email  (:commit/author-email commit)
        name   (:commit/author-name commit)
        date   (:commit/author-date commit)]
    (-> acc
        (update :total inc)
        (update-in [:authors email]
                   (fn [cur]
                     (let [cur    (or cur {:commits 0
                                           :name name
                                           :first-seen date
                                           :last-seen date})
                           newer? (and date (:last-seen cur)
                                       (not (neg? (compare date (:last-seen cur)))))]
                       (-> cur
                           (update :commits inc)
                           (update :first-seen
                                   (fn [old]
                                     (if (and old date (neg? (compare old date)))
                                       old date)))
                           ;; A newer-or-equal commit updates :last-seen and :name together,
                           ;; so the two can never drift out of agreement.
                           (cond->
                            newer? (assoc :last-seen date :name name)))))))))

(def accumulate-step step-author)

(defn finalize-authors
  "Finalize accumulated state into the standard author-stats result map."
  [acc]
  (let [total (:total acc)]
    {:total-commits total
     :total-authors (count (:authors acc))
     :rows (->> (:authors acc)
                (map (fn [[_email stats]]
                       {:author     (:name stats)
                        :commits    (:commits stats)
                        :percent    (util/pct (:commits stats) total)
                        :first-seen (:first-seen stats)
                        :last-seen  (:last-seen stats)}))
                (sort-by (comp - :commits))
                vec)}))

(defn author-stats
  "Reduce commits into author statistics.
   When :path is set, only commits touching at least one matching file are counted.
   Returns {:total-commits n :rows [{:author :commits :percent :first-seen :last-seen}]}"
  [commits opts]
  (finalize-authors (reduce accumulate-step (init-acc)
                            (filters/filter-commits-by-path commits (:path opts)))))

;; ---------------------------------------------------------------------------
;; .mailmap suggestions — cluster likely-duplicate identities so a repo owner
;; can curate a .mailmap (the same aliases-collapse-to-one-identity job
;; GitHub's contributor graph does at the account level, see gitsnitch.git).
;; ---------------------------------------------------------------------------

(defn- normalize-name
  [name]
  (-> (or name "") str/lower-case str/trim (str/replace #"\s+" " ")))

(defn- normalize-email-local
  "The email's local part (before @), lower-cased with punctuation stripped,
   so simon.neutert / simonneutert / Simon.Neutert count as the same handle."
  [email]
  (-> (or email "") (str/split #"@") first str/lower-case (str/replace #"[^a-z0-9]" "")))

(defn- index-by
  "Group identity indices by key-fn's value, dropping the blank-key group
   (blank names/emails must never force unrelated identities together)."
  [key-fn identities]
  (->> identities
       (map-indexed vector)
       (reduce (fn [groups [i identity]]
                 (let [k (key-fn identity)]
                   (if (seq k) (update groups k (fnil conj []) i) groups)))
               {})
       vals))

(defn- add-group-edges
  "Wire every index in group to every other index in group."
  [adj group]
  (reduce (fn [adj i] (update adj i into (remove #(= % i) group))) adj group))

(defn- connected-components
  "Group identity indices into connected components: two identities are
   linked when they share a normalized name or a normalized email local-part
   (transitive: a~b, b~c => a,b,c grouped). Building the adjacency by
   grouping on those two keys, rather than comparing every pair, keeps this
   O(n) instead of O(n²) — important once a repo has hundreds of authors."
  [identities]
  (let [n        (count identities)
        by-name  (index-by (comp normalize-name :name) identities)
        by-email (index-by (comp normalize-email-local :email) identities)
        adj      (as-> (vec (repeat n #{})) adj
                   (reduce add-group-edges adj by-name)
                   (reduce add-group-edges adj by-email))]
    (loop [remaining (set (range n)) components []]
      (if (empty? remaining)
        components
        (let [start     (first remaining)
              component (loop [visited #{start} frontier [start]]
                          (if (empty? frontier)
                            visited
                            (let [next-frontier (remove visited (mapcat adj frontier))]
                              (recur (into visited next-frontier) next-frontier))))]
          (recur (apply disj remaining component) (conj components component)))))))

(defn- mailmap-line
  [canonical alias]
  (str (:name canonical) " <" (:email canonical) ">"
       (if (= (normalize-name (:name alias)) (normalize-name (:name canonical)))
         (str " <" (:email alias) ">")
         (str " " (:name alias) " <" (:email alias) ">"))))

(defn- canonical-order
  "Comparator picking the best canonical identity first: most commits, then
   most-recently-seen (a tie should favor the still-active email over an
   abandoned one), then email as a final deterministic tie-break."
  [a b]
  (let [by-commits   (compare (:commits b) (:commits a))
        by-last-seen (fn [] (compare (or (:last-seen b) "") (or (:last-seen a) "")))
        by-email     (fn [] (compare (:email a) (:email b)))]
    (if (not= 0 by-commits)
      by-commits
      (let [c (by-last-seen)]
        (if (not= 0 c) c (by-email))))))

(defn suggest-mailmap
  "Cluster an author-acc's :authors map ({email {:name :commits :last-seen}})
   into likely-duplicate identities. Returns a vector of
   {:canonical {:name :email :commits} :aliases [{:name :email :commits} ...]
    :mailmap-lines [str ...]}, most-commits-first, singletons omitted."
  [authors-map]
  (let [identities (mapv (fn [[email stats]]
                           {:email email :name (:name stats) :commits (:commits stats)
                            :last-seen (:last-seen stats)})
                         authors-map)
        components (connected-components identities)]
    (->> components
         (filter #(> (count %) 1))
         (map (fn [idx-set]
                (let [members    (map (partial nth identities) idx-set)
                      sorted     (sort canonical-order members)
                      canonical  (first sorted)
                      aliases    (rest sorted)]
                  {:canonical      canonical
                   :aliases        (vec aliases)
                   :mailmap-lines  (mapv #(mailmap-line canonical %) aliases)})))
         (sort-by (comp - :commits :canonical))
         vec)))

(defn concentration-warnings
  "Produce bus-factor warnings from author stats rows."
  [rows _total-commits]
  (let [top1-pct  (when (seq rows) (:percent (first rows)))
        top3-pct  (when (>= (count rows) 3)
                    (reduce + (map :percent (take 3 rows))))]
    (cond-> []
      (and top1-pct (> top1-pct 60.0))
      (conj {:level :severe
             :message (str "Single author (" (:author (first rows))
                           ") has " top1-pct "% of commits — severe concentration risk")})

      (and top3-pct (> top3-pct 80.0))
      (conj {:level :warning
             :message (str "Top 3 authors account for " (format "%.1f" top3-pct)
                           "% of commits — strong concentration warning")}))))
