(ns gitsnitch.cache
  "Cache helper for the babashka-clone smoke suite. Clones
   github.com/babashka/babashka (main branch) once into a project-local,
   gitignored .cache/ dir, then reuses it across test runs without ever
   auto-pulling — so smoke-test content can't drift mid-suite. Callers use
   `ensure-clone!` and treat a nil return as \"skip, don't fail\" (no
   network and no existing cache)."
  (:require [babashka.fs :as fs]
            [babashka.process :as p]
            [clojure.string :as str]))

(def cache-root (fs/path ".cache"))
(def clone-name "gitsnitch-testing-babashka-main-clone")
(def clone-dir (fs/path cache-root clone-name))
(def lock-file (fs/path cache-root (str clone-name ".lock")))
(def clone-url "https://github.com/babashka/babashka.git")
(def shallow-depth 300)

(defn online?
  "Best-effort connectivity probe; never throws."
  []
  (let [result (p/shell {:out :string :err :string :continue true :timeout 5000}
                        "git" "ls-remote" "--exit-code" clone-url "HEAD")]
    (zero? (:exit result))))

(defn cached?
  "True when clone-dir exists and looks like a real git repo."
  []
  (and (fs/exists? clone-dir)
       (fs/exists? (fs/path clone-dir ".git"))))

(defn- shallow-clone-to!
  "Shallow-clone the repo's default branch (whatever it's actually named —
   babashka's is `master`, not `main`), so this doesn't assume a branch name."
  [target]
  (let [result (p/shell {:out :string :err :string :continue true}
                        "git" "clone" "--depth" (str shallow-depth)
                        "--single-branch"
                        clone-url (str target))]
    (when-not (zero? (:exit result))
      (throw (ex-info "git clone failed" result)))))

(defn- with-lock
  "Cross-process advisory lock via atomic lockfile creation. Retries with
   backoff up to timeout-ms, then throws."
  [timeout-ms f]
  (fs/create-dirs cache-root)
  (let [deadline (+ (System/currentTimeMillis) timeout-ms)]
    (loop []
      (if (try (fs/create-file lock-file) true
               (catch java.nio.file.FileAlreadyExistsException _ false))
        (try (f) (finally (fs/delete-if-exists lock-file)))
        (if (> (System/currentTimeMillis) deadline)
          (throw (ex-info "timed out waiting for babashka-clone lock"
                          {:lock-file (str lock-file)}))
          (do (Thread/sleep 500) (recur)))))))

(defn- clone-into-cache! []
  (with-lock 120000
    (fn []
      (if (cached?) ;; another process may have finished while we waited
        clone-dir
        (let [tmp (fs/create-temp-dir {:dir (str cache-root) :prefix "clone-tmp-"})]
          (shallow-clone-to! tmp)
          (fs/delete-if-exists clone-dir)
          (fs/move tmp clone-dir {:atomic-move true :replace-existing true})
          clone-dir)))))

(defn ensure-clone!
  "Return the cache dir path if usable. An existing cache is reused as-is
   and never auto-pulled. Returns nil (never throws) when offline and no
   cache exists yet — callers use this to skip gracefully."
  []
  (cond
    (cached?)        clone-dir
    (not (online?))  nil
    :else            (clone-into-cache!)))

(defn refresh!
  "Explicit-only entrypoint: deletes the existing cache and re-clones.
   Never called implicitly by the smoke tests."
  []
  (with-lock 120000
    (fn []
      (fs/delete-tree clone-dir {:force true})
      (let [tmp (fs/create-temp-dir {:dir (str cache-root) :prefix "clone-tmp-"})]
        (shallow-clone-to! tmp)
        (fs/move tmp clone-dir {:atomic-move true :replace-existing true})
        (println "Refreshed" (str clone-dir))))))
