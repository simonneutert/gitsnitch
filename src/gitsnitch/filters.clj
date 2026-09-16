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
   Glob matchers are compiled once when the filter is created.

   A pattern with no `/` (e.g. \"*.lock\") matches the path's basename
   anywhere in the tree, gitignore-style — a bare `*` never crosses a `/`
   under java.nio.file's glob semantics, so without this a pattern like
   \"*.lock\" would silently only match root-level files. A pattern
   containing `/` (e.g. \"vendor/*\") keeps matching the full relative path,
   scoped exactly as written."
  [exclude-patterns]
  (if (seq exclude-patterns)
    (let [matchers (mapv (fn [pattern]
                           {:scoped? (str/includes? pattern "/")
                            :matcher (glob-matcher pattern)})
                         exclude-patterns)]
      (fn [path]
        (let [full (java.nio.file.Path/of path (into-array String []))
              base (.getFileName full)]
          (not (some (fn [{:keys [scoped? matcher]}]
                       (.matches ^java.nio.file.PathMatcher matcher
                                 (if scoped? full base)))
                     matchers)))))
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