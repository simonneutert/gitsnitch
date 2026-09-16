(ns gitsnitch.fixtures
  "Test-only helpers: synthetic on-disk git repos for integration tests, and
   fixture commit maps (matching gitsnitch.git's parsed commit shape) for
   unit tests that don't need a real repo at all."
  (:require [babashka.fs :as fs]
            [babashka.process :as p]
            [clojure.string :as str]))

;; ---------------------------------------------------------------------------
;; Synthetic on-disk repos
;; ---------------------------------------------------------------------------

(defn sh!
  "Run a shell command in dir. Throws ex-info with the full result on failure."
  [dir & args]
  (let [result (apply p/shell {:dir (str dir) :out :string :err :string :continue true} args)]
    (when-not (zero? (:exit result))
      (throw (ex-info (str "command failed: " (str/join " " args)) result)))
    result))

(defn init-repo!
  "Create a tmp dir, `git init` it, and set deterministic local config.
   Returns the repo dir path."
  []
  (let [dir (fs/create-temp-dir {:prefix "gitsnitch-test-"})]
    (sh! dir "git" "init" "-q" "-b" "main")
    (sh! dir "git" "config" "user.name" "Test User")
    (sh! dir "git" "config" "user.email" "test@example.com")
    (sh! dir "git" "config" "commit.gpgsign" "false")
    dir))

(defn- write-file! [dir path content]
  (let [f (fs/path dir path)]
    (fs/create-dirs (fs/parent f))
    (spit (fs/file f) content)))

(defn commit!
  "Apply one commit step to dir and return the new commit sha.
   step is a map:
     {:files   {\"a.clj\" \"content\"}   ; path -> content, written/overwritten
      :delete  [\"old.clj\"]             ; optional, paths removed before commit
      :message \"fix: repair the thing\"
      :author  {:name \"Alice\" :email \"alice@example.com\"}  ; optional
      :date    \"2024-01-15T10:00:00\"}  ; optional, sets both author + committer date"
  [dir {:keys [files delete message author date]}]
  (doseq [[path content] files] (write-file! dir path content))
  (doseq [path delete] (fs/delete-if-exists (fs/path dir path)))
  (sh! dir "git" "add" "-A")
  (let [extra-env (cond-> {}
                    date   (assoc "GIT_AUTHOR_DATE" date "GIT_COMMITTER_DATE" date)
                    author (into {"GIT_AUTHOR_NAME"  (:name author)
                                  "GIT_AUTHOR_EMAIL" (:email author)}))
        result (p/shell {:dir (str dir) :extra-env extra-env
                         :out :string :err :string :continue true}
                        "git" "commit" "-q" "-m" message)]
    (when-not (zero? (:exit result))
      (throw (ex-info "git commit failed" result))))
  (str/trim (:out (sh! dir "git" "rev-parse" "HEAD"))))

(defn build-repo!
  "Build a repo from an ordered seq of commit-step maps (see `commit!`).
   Returns the repo dir path."
  [commit-steps]
  (let [dir (init-repo!)]
    (run! #(commit! dir %) commit-steps)
    dir))

(defn branch! [dir name]
  (sh! dir "git" "checkout" "-q" "-b" name))

(defn checkout! [dir name]
  (sh! dir "git" "checkout" "-q" name))

(defn merge-branch!
  "Merge branch into current HEAD with --no-ff, guaranteeing a real merge commit."
  [dir branch & {:keys [message] :or {message (str "Merge branch '" branch "'")}}]
  (sh! dir "git" "merge" "-q" "--no-ff" "-m" message branch))

(defn delete-repo! [dir]
  (fs/delete-tree dir {:force true}))

(def project-src
  "Absolute path to this project's src/ dir, resolved once at load time (bb
   tasks run with cwd = project root) so subprocess CLI invocations can find
   gitsnitch's source regardless of what :dir they're run against."
  (str (fs/canonicalize "src")))

(defn run-gitsnitch
  "Run `bb -m gitsnitch.main <args>` as a subprocess with cwd set to dir, so
   the CLI-under-test operates on that repo (gitsnitch has no --repo/--cwd
   flag — it always reads the process's inherited cwd). Returns the
   babashka.process/shell result map ({:keys [exit out err]}), never throws."
  [dir & args]
  (apply p/shell {:dir (str dir) :out :string :err :string :continue true}
         "bb" "-cp" project-src "-m" "gitsnitch.main" args))

;; ---------------------------------------------------------------------------
;; Fixture commit maps (pure, no real git) — matches gitsnitch.git's parsed shape
;; ---------------------------------------------------------------------------

(defn commit-map
  "A commit map matching gitsnitch.git's parse-commit-header/finalize-commit
   output, with sensible defaults merged with overrides."
  [overrides]
  (merge {:commit/hash           "aaaa1111aaaa1111aaaa1111aaaa1111aaaa1111"
          :commit/short          "aaaa111"
          :commit/author-name    "Alice"
          :commit/author-email   "alice@example.com"
          :commit/author-date    "2024-01-15T10:00:00+01:00"
          :commit/committer-name "Alice"
          :commit/subject        "add feature"
          :commit/parents        []
          :commit/merge?         false
          :commit/files          ["a.clj"]
          :commit/file-count     1}
         overrides))

(defn numstat-commit
  "Like commit-map, but adds :commit/numstat for :detailed? metric paths."
  [overrides numstat-rows]
  (assoc (commit-map overrides) :commit/numstat numstat-rows))
