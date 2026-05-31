(ns gitsnitch.filters
  (:require [clojure.string :as str])
  (:import [java.nio.file FileSystems]))

(defn- glob-matcher
  "Create a PathMatcher for a glob pattern."
  [pattern]
  (.getPathMatcher (FileSystems/getDefault) (str "glob:" pattern)))

(defn exclude-filter
  "Returns a predicate that returns true if a path should be KEPT
   (i.e., does NOT match any exclude pattern).
   Glob matchers are compiled once when the filter is created."
  [exclude-patterns]
  (if (seq exclude-patterns)
    (let [matchers (mapv glob-matcher exclude-patterns)]
      (fn [path]
        (let [p (java.nio.file.Path/of path (into-array String []))]
          (not (some #(.matches ^java.nio.file.PathMatcher % p) matchers)))))
    (constantly true)))

(defn path-filter
  "Returns a predicate that returns true if a path starts with any of the given prefixes."
  [path-prefixes]
  (if (seq path-prefixes)
    (fn [file-path]
      (some #(str/starts-with? file-path %) path-prefixes))
    (constantly true)))

(defn filter-commits-by-path
  "Filter commits to those touching at least one file matching path prefixes."
  [commits path-prefixes]
  (if (seq path-prefixes)
    (let [pred (path-filter path-prefixes)]
      (filter #(some pred (:commit/files %)) commits))
    commits))

(comment
  ;; not used yet, but could be useful for future optimizations
  (defn filter-files
    "Filter commit files by path prefixes and exclude patterns."
    [files {:keys [path exclude]}]
    (let [keep-path? (path-filter path)
          keep-excl? (exclude-filter exclude)]
      (filterv #(and (keep-path? %) (keep-excl? %)) files))))