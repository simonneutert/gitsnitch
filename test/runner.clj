#!/usr/bin/env bb
;; Discovers and runs clojure.test namespaces under the given root dirs.
;; Usage: bb -cp src:test test/runner.clj <root-dir> [<root-dir> ...]

(require '[babashka.fs :as fs]
         '[clojure.string :as str]
         '[clojure.test :as t])

(def test-root (fs/path "test"))

(defn- path->ns [path]
  (-> (str (fs/relativize test-root path))
      (str/replace #"\.clj$" "")
      (str/replace "/" ".")
      (str/replace "_" "-")
      symbol))

(defn- discover [dir]
  ;; babashka.fs/glob's "**/" doesn't match zero intermediate directories,
  ;; so root-level and nested test files need separate glob alternatives.
  (->> (fs/glob dir "{*_test.clj,**/*_test.clj}")
       (map path->ns)))

(let [dirs (seq *command-line-args*)]
  (when-not dirs
    (binding [*out* *err*]
      (println "usage: runner.clj <root-dir> [<root-dir> ...]"))
    (System/exit 1))
  (let [nses (distinct (mapcat discover dirs))]
    (when (empty? nses)
      (binding [*out* *err*]
        (println "no *_test.clj namespaces found under" (str/join ", " dirs))))
    (run! require nses)
    (let [{:keys [fail error]} (apply t/run-tests nses)]
      (System/exit (if (zero? (+ (or fail 0) (or error 0))) 0 1)))))
