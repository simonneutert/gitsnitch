(ns gitsnitch.unit.metrics.authors-test
  (:require [clojure.test :refer [deftest is testing]]
            [gitsnitch.metrics.authors :as authors]
            [gitsnitch.fixtures :as fx]))

(defn- commits []
  [(fx/commit-map {:commit/author-email "a@x.com" :commit/author-name "Alice"
                   :commit/author-date "2024-01-01T00:00:00Z"})
   (fx/commit-map {:commit/author-email "a@x.com" :commit/author-name "Alice"
                   :commit/author-date "2024-01-02T00:00:00Z"})
   (fx/commit-map {:commit/author-email "b@x.com" :commit/author-name "Bob"
                   :commit/author-date "2024-01-03T00:00:00Z"})])

(deftest author-stats-totals-and-ranking
  (let [result (authors/author-stats (commits) {})]
    (is (= 3 (:total-commits result)))
    (is (= 2 (:total-authors result)))
    (is (= ["Alice" "Bob"] (map :author (:rows result))))
    (is (= 2 (:commits (first (:rows result)))))))

(deftest author-stats-first-seen-last-seen
  (let [result (authors/author-stats (commits) {})
        alice (first (:rows result))]
    (is (= "2024-01-01T00:00:00Z" (:first-seen alice)))
    (is (= "2024-01-02T00:00:00Z" (:last-seen alice)))))

(deftest author-stats-path-filter
  (let [commits [(fx/commit-map {:commit/author-email "a@x.com" :commit/files ["src/a.clj"]})
                 (fx/commit-map {:commit/author-email "b@x.com" :commit/files ["docs/readme.md"]})]
        result (authors/author-stats commits {:path ["src/"]})]
    (is (= 1 (:total-commits result)))
    (is (= 1 (:total-authors result)))))

(deftest concentration-warnings-severe-single-author
  (let [rows [{:author "Alice" :commits 8 :percent 80.0}
              {:author "Bob" :commits 2 :percent 20.0}]
        warnings (authors/concentration-warnings rows 10)]
    (is (= :severe (:level (first warnings))))
    (is (re-find #"Alice" (:message (first warnings))))))

(deftest concentration-warnings-top3-warning
  (let [rows [{:author "A" :commits 3 :percent 30.0}
              {:author "B" :commits 3 :percent 30.0}
              {:author "C" :commits 3 :percent 30.0}
              {:author "D" :commits 1 :percent 10.0}]
        warnings (authors/concentration-warnings rows 10)]
    (is (some #(= :warning (:level %)) warnings))))

(deftest concentration-warnings-none-when-balanced
  (let [rows [{:author "A" :commits 5 :percent 50.0}
              {:author "B" :commits 5 :percent 50.0}]]
    (is (empty? (authors/concentration-warnings rows 10)))))

(deftest concentration-warnings-empty-rows
  (is (empty? (authors/concentration-warnings [] 0))))

(deftest suggest-mailmap-clusters-by-matching-name
  (let [authors-map {"simon.neutert@gmail.com"
                     {:name "Simon" :commits 30 :last-seen "2019-01-01"}
                     "simonneutert@users.noreply.github.com"
                     {:name "Simon Neutert" :commits 84 :last-seen "2026-01-01"}
                     "bob@x.com"
                     {:name "Bob" :commits 5 :last-seen "2020-01-01"}}
        clusters (authors/suggest-mailmap authors-map)]
    (is (= 1 (count clusters)))
    (is (= "Simon Neutert" (:name (:canonical (first clusters)))))
    (is (= 84 (:commits (:canonical (first clusters)))))
    (is (= ["simon.neutert@gmail.com"] (map :email (:aliases (first clusters)))))))

(deftest suggest-mailmap-clusters-by-matching-email-local-part
  (let [clusters (authors/suggest-mailmap
                  {"simon.neutert@work.com" {:name "S. Neutert" :commits 10 :last-seen "2020-01-01"}
                   "simonneutert@gmail.com" {:name "Someone Else" :commits 1 :last-seen "2018-01-01"}})]
    (is (= 1 (count clusters)))))

(deftest suggest-mailmap-no-duplicates-returns-empty
  (let [authors-map {"alice@x.com" {:name "Alice" :commits 2 :last-seen "2024-01-01"}
                     "bob@x.com"   {:name "Bob" :commits 1 :last-seen "2024-01-02"}}]
    (is (empty? (authors/suggest-mailmap authors-map)))))

(deftest suggest-mailmap-line-uses-bare-email-when-names-match
  (let [authors-map {"simonneutert@users.noreply.github.com"
                     {:name "Simon Neutert" :commits 84 :last-seen "2026-01-01"}
                     "simon@fritz.box"
                     {:name "Simon Neutert" :commits 5 :last-seen "2019-01-01"}}
        line (first (:mailmap-lines (first (authors/suggest-mailmap authors-map))))]
    (is (= "Simon Neutert <simonneutert@users.noreply.github.com> <simon@fritz.box>" line))))

(deftest suggest-mailmap-line-includes-alias-name-when-names-differ
  (let [authors-map {"simonneutert@users.noreply.github.com"
                     {:name "Simon Neutert" :commits 84 :last-seen "2026-01-01"}
                     "simon.neutert@gmail.com"
                     {:name "Simon" :commits 30 :last-seen "2019-01-01"}}
        line (first (:mailmap-lines (first (authors/suggest-mailmap authors-map))))]
    (is (= "Simon Neutert <simonneutert@users.noreply.github.com> Simon <simon.neutert@gmail.com>" line))))

(deftest suggest-mailmap-tie-break-prefers-newer-identity-as-canonical
  (testing "equal commit counts should favor the still-active (newer) email
            as canonical, not an abandoned older one"
    (let [authors-map {"old@x.com" {:name "Simon" :commits 5 :last-seen "2019-01-01"}
                       "new@x.com" {:name "Simon" :commits 5 :last-seen "2026-01-01"}}
          canonical (:canonical (first (authors/suggest-mailmap authors-map)))]
      (is (= "new@x.com" (:email canonical))))))

(deftest suggest-mailmap-tie-break-falls-back-to-email-when-fully-tied
  (let [authors-map {"a@x.com" {:name "Simon" :commits 5 :last-seen "2020-01-01"}
                     "b@x.com" {:name "Simon" :commits 5 :last-seen "2020-01-01"}}
        canonical (:canonical (first (authors/suggest-mailmap authors-map)))]
    (is (= "a@x.com" (:email canonical)))))
