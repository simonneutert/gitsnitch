(ns gitsnitch.unit.git-test
  "gitsnitch.git's arg-builders and stream parsers are pure but private —
   reached here via #'var deref, the idiomatic clojure.test way to test
   private fns without changing their visibility."
  (:require [clojure.test :refer [deftest is testing]]
            [gitsnitch.git :as git]))

(def build-rev-list-args #'git/build-rev-list-args)
(def build-log-args #'git/build-log-args)
(def parse-commit-header #'git/parse-commit-header)
(def parse-numstat-line #'git/parse-numstat-line)
(def finalize-commit #'git/finalize-commit)
(def parse-log-stream #'git/parse-log-stream)

(deftest build-rev-list-args-defaults
  (is (= ["rev-list" "HEAD" "--count"]
         (conj (build-rev-list-args {}) "--count"))))

(deftest build-rev-list-args-with-opts
  (is (= ["rev-list" "abc..def" "--since=1.year.ago" "--until=1.day.ago"
          "--no-merges" "--first-parent" "--max-count=5"]
         (build-rev-list-args {:rev "abc..def" :since "1.year.ago" :until "1.day.ago"
                               :no-merges true :first-parent true :limit 5}))))

(deftest build-log-args-name-only-vs-numstat
  (testing "non-detailed uses --name-only"
    (is (some #{"--name-only"} (build-log-args {} false))))
  (testing "detailed uses --numstat"
    (is (some #{"--numstat"} (build-log-args {} true)))
    (is (not (some #{"--name-only"} (build-log-args {} true))))))

(deftest parse-commit-header-basic
  (let [line (str "abc123abc12Alicealice@x.com"
                  "2024-01-15T10:00:00+01:00Aliceadd feature")]
    (is (= {:commit/hash "abc123" :commit/short "abc12"
            :commit/author-name "Alice" :commit/author-email "alice@x.com"
            :commit/author-date "2024-01-15T10:00:00+01:00"
            :commit/committer-name "Alice" :commit/subject "add feature"
            :commit/parents [] :commit/merge? false}
           (parse-commit-header line)))))

(deftest parse-commit-header-merge-commit
  (let [line (str "abcabcAlicealice@x.com"
                  "2024-01-15T10:00:00+01:00AliceMergeparent1 parent2")]
    (is (true? (:commit/merge? (parse-commit-header line))))
    (is (= ["parent1" "parent2"] (:commit/parents (parse-commit-header line))))))

(deftest parse-commit-header-too-few-fields-returns-nil
  (is (nil? (parse-commit-header "onlytwo"))))

(deftest parse-numstat-line-basic
  (is (= {:path "a.clj" :insertions 3 :deletions 1}
         (parse-numstat-line "3\t1\ta.clj"))))

(deftest parse-numstat-line-binary-file-dashes-become-zero
  (is (= {:path "img.png" :insertions 0 :deletions 0}
         (parse-numstat-line "-\t-\timg.png"))))

(deftest finalize-commit-name-only
  (let [commit {:commit/hash "abc"}
        result (finalize-commit commit ["a.clj" "" "b.clj"] false)]
    (is (= ["a.clj" "b.clj"] (:commit/files result)))
    (is (= 2 (:commit/file-count result)))))

(deftest finalize-commit-detailed
  (let [commit {:commit/hash "abc"}
        result (finalize-commit commit ["3\t1\ta.clj" "0\t2\tb.clj"] true)]
    (is (= ["a.clj" "b.clj"] (:commit/files result)))
    (is (= [{:path "a.clj" :insertions 3 :deletions 1}
            {:path "b.clj" :insertions 0 :deletions 2}]
           (:commit/numstat result)))))

(deftest finalize-commit-nil-commit-returns-nil
  (is (nil? (finalize-commit nil [] false))))

(deftest parse-log-stream-single-commit
  (let [header (str "abcabcAlicealice@x.com"
                    "2024-01-15T10:00:00+01:00Aliceadd feature")
        lines [header "a.clj" "b.clj"]
        result (doall (parse-log-stream lines false))]
    (is (= 1 (count result)))
    (is (= ["a.clj" "b.clj"] (:commit/files (first result))))))

(deftest parse-log-stream-multiple-commits
  (let [h1 (str "aaaaaaAlicealice@x.com"
                "2024-01-01T10:00:00+01:00Alicefirst")
        h2 (str "bbbbbbBobbob@x.com"
                "2024-01-02T10:00:00+01:00Bobsecond")
        lines [h1 "a.clj" h2 "b.clj"]
        result (doall (parse-log-stream lines false))]
    (is (= 2 (count result)))
    (is (= ["aaa" "bbb"] (map :commit/hash result)))
    (is (= [["a.clj"] ["b.clj"]] (map :commit/files result)))))
