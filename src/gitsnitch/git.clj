(ns gitsnitch.git
  (:require [babashka.process :as p]
            [clojure.string :as str]
            [clojure.java.io :as io]))

(defn repo-root
  "Returns the Git repo root directory, or nil if not in a repo."
  []
  (let [result (p/shell {:out :string :err :string :continue true}
                        "git" "rev-parse" "--show-toplevel")]
    (when (zero? (:exit result))
      (str/trim (:out result)))))

(defn- run-git
  "Run a git command, return stdout as string. Throws on failure."
  [& args]
  (let [cmd    (into ["git"] (flatten args))
        result (apply p/shell {:out :string :err :string :continue true} cmd)]
    (when-not (zero? (:exit result))
      (throw (ex-info (str "git failed: " (str/trim (:err result)))
                      {:args args :exit (:exit result)})))
    (str/trim (:out result))))

;; ---------------------------------------------------------------------------
;; Commit count (fast, for progress display)
;; ---------------------------------------------------------------------------

(defn- build-rev-list-args
  "Build git rev-list arguments from opts."
  [{:keys [rev since until no-merges first-parent limit]}]
  (cond-> []
    true          (conj "rev-list")
    (not rev)     (conj "HEAD")
    rev           (conj rev)
    since         (conj (str "--since=" since))
    until         (conj (str "--until=" until))
    no-merges     (conj "--no-merges")
    first-parent  (conj "--first-parent")
    limit         (conj (str "--max-count=" limit))))

(defn commit-count
  "Return the number of commits matching opts (fast, uses --count)."
  [opts]
  (let [args (-> (build-rev-list-args opts)
                 (conj "--count"))
        out  (apply run-git args)]
    (parse-long (str/trim out))))

;; ---------------------------------------------------------------------------
;; Streaming git-log parser — single process for all commit data
;; ---------------------------------------------------------------------------

(def ^:private commit-format
  ;; Record-separator prefix, then fields separated by unit-separator
  ;; Hash, short hash, author name, author email, author date (ISO),
  ;; committer name, subject, parent hashes
  "%x1e%H%x1f%h%x1f%an%x1f%ae%x1f%aI%x1f%cn%x1f%s%x1f%P")

(defn- build-log-args
  "Build git log arguments from opts."
  [{:keys [rev since until no-merges first-parent limit]} detailed?]
  (cond-> ["log" (str "--format=" commit-format)]
    (not rev)     (conj "HEAD")
    rev           (conj rev)
    since         (conj (str "--since=" since))
    until         (conj (str "--until=" until))
    no-merges     (conj "--no-merges")
    first-parent  (conj "--first-parent")
    limit         (conj (str "--max-count=" limit))
    (not detailed?) (conj "--name-only")
    detailed?     (conj "--numstat")
    true          (conj "-r" "--root")))

(defn- parse-commit-header
  "Parse a commit header line (starts after \\x1e) into a commit map."
  [line]
  (let [raw    (if (str/starts-with? line "\u001e")
                 (subs line 1)
                 line)
        fields (str/split raw #"\u001f" -1)]
    (when (>= (count fields) 7)
      (let [[h short-h author-name author-email author-date
             committer-name subject] fields
            parents-str (when (>= (count fields) 8) (nth fields 7))
            parents (when (and parents-str (not (str/blank? parents-str)))
                      (str/split parents-str #" "))]
        {:commit/hash           h
         :commit/short          short-h
         :commit/author-name    author-name
         :commit/author-email   author-email
         :commit/author-date    author-date
         :commit/committer-name committer-name
         :commit/subject        subject
         :commit/parents        (or parents [])
         :commit/merge?         (> (count (or parents [])) 1)}))))

(defn- parse-numstat-line
  "Parse a --numstat line: insertions<TAB>deletions<TAB>path."
  [line]
  (let [parts (str/split line #"\t" 3)]
    (when (= 3 (count parts))
      (let [[ins del path] parts]
        {:path       path
         :insertions (if (= ins "-") 0 (parse-long ins))
         :deletions  (if (= del "-") 0 (parse-long del))}))))

(defn- finalize-commit
  "Attach accumulated file data to a commit map."
  [commit file-lines detailed?]
  (when commit
    (if detailed?
      (let [numstat (vec (keep parse-numstat-line file-lines))
            files   (mapv :path numstat)]
        (assoc commit
               :commit/files files
               :commit/file-count (count files)
               :commit/numstat numstat))
      (let [files (vec (remove str/blank? file-lines))]
        (assoc commit
               :commit/files files
               :commit/file-count (count files))))))

(defn- parse-log-stream
  "Parse a lazy line-seq from git-log into a lazy seq of commit maps.
   Uses \\x1e as the commit-header sentinel."
  ([lines detailed?]
   (parse-log-stream lines detailed? nil []))
  ([lines detailed? cur-commit cur-files]
   (lazy-seq
    (loop [lines      lines
           cur-commit cur-commit
           cur-files  cur-files]
      (if-let [line (first lines)]
        (if (str/starts-with? line "\u001e")
          ;; New commit header — emit previous commit (if any), start new one
          (let [prev (finalize-commit cur-commit cur-files detailed?)
                next-commit (parse-commit-header line)]
            (if prev
              (cons prev (parse-log-stream (rest lines) detailed? next-commit []))
              (recur (rest lines) next-commit [])))
          ;; File/numstat line — accumulate
          (recur (rest lines) cur-commit (conj cur-files line)))
        ;; End of stream — emit final commit
        (when-let [final (finalize-commit cur-commit cur-files detailed?)]
          (list final)))))))

(defn- start-git-log
  "Start a git-log process, return a lazy seq of commit maps.
   The process stdout is streamed, not buffered."
  [opts detailed?]
  (let [args    (build-log-args opts detailed?)
        cmd     (into ["git"] args)
        proc    (apply p/process {:out :stream :err :inherit :shutdown p/destroy-tree} cmd)
        reader  (io/reader (:out proc))
        lines   (line-seq reader)]
    (parse-log-stream lines detailed?)))

(defn stream-commits
  "Return a lazy seq of full commit maps (with :commit/files) for opts.
   Uses a single streaming git-log process."
  [opts]
  (start-git-log opts false))

(defn stream-commits-detailed
  "Like stream-commits but includes numstat (insertions/deletions per file)."
  [opts]
  (start-git-log opts true))
