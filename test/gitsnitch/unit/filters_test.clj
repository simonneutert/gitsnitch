(ns gitsnitch.unit.filters-test
  (:require [clojure.test :refer [deftest is testing]]
            [gitsnitch.filters :as filters]
            [gitsnitch.fixtures :as fx]))

(deftest exclude-filter-no-patterns-keeps-everything
  (is (true? ((filters/exclude-filter nil) "anything.clj")))
  (is (true? ((filters/exclude-filter []) "anything.clj"))))

(deftest exclude-filter-glob-matching
  (let [keep? (filters/exclude-filter ["*.lock" "vendor/*"])]
    (testing "matching patterns are excluded (predicate returns false)"
      (is (false? (keep? "package.lock")))
      (is (false? (keep? "vendor/foo.clj"))))
    (testing "non-matching paths are kept"
      (is (true? (keep? "src/core.clj"))))))

(deftest exclude-filter-slash-free-pattern-crosses-directories
  (testing "a bare glob with no / matches the basename anywhere in the tree, gitignore-style"
    (let [keep? (filters/exclude-filter ["*.lock"])]
      (is (false? (keep? "package.lock")))
      (is (false? (keep? "vendor/foo.lock")))
      (is (false? (keep? "deeply/nested/dir/bar.lock")))
      (is (true? (keep? "src/core.clj"))))))

(deftest exclude-filter-scoped-pattern-stays-scoped
  (testing "a pattern containing / only matches that exact relative path, not any nested occurrence"
    (let [keep? (filters/exclude-filter ["vendor/*"])]
      (is (false? (keep? "vendor/foo.clj")))
      (is (true? (keep? "src/vendor/foo.clj")))
      (is (true? (keep? "vendor/nested/foo.clj"))))))

(deftest path-filter-prefix-matching
  (let [keep? (filters/path-filter ["src/"])]
    (is (true? (keep? "src/core.clj")))
    (is (not (keep? "test/core_test.clj")))))

(deftest path-filter-empty-prefixes-keeps-everything
  (is (true? ((filters/path-filter nil) "anything.clj")))
  (is (true? ((filters/path-filter []) "anything.clj"))))

(deftest filter-commits-by-path-no-prefixes-returns-unchanged
  (let [commits [(fx/commit-map {:commit/files ["a.clj"]})]]
    (is (= commits (filters/filter-commits-by-path commits nil)))))

(deftest filter-commits-by-path-keeps-only-matching-commits
  (let [a (fx/commit-map {:commit/files ["src/a.clj"]})
        b (fx/commit-map {:commit/files ["test/b.clj"]})
        result (filters/filter-commits-by-path [a b] ["src/"])]
    (is (= [a] result))))
