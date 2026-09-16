# Developing gitsnitch

## Running commands during development

```sh
bb summary
bb churn --since 1.year.ago
bb authors
bb activity --by week
bb coupling --min-cochanges 3
bb bugs
bb danger
```

## Tests

The test suite has two layers:

### `bb test` — fast, offline, runs by default

```sh
bb test
```

Covers `test/gitsnitch/unit/` (pure functions, mirroring `src/gitsnitch/*`
one-to-one, exercised with hand-built fixture data — no real git repo involved)
and `test/gitsnitch/integration/` (the actual CLI, run as a subprocess against
small synthetic repos built on the fly with `git init` + scripted commits via
`test/gitsnitch/fixtures.clj`). Both are fully deterministic and require no
network — this is the suite CI and local dev should run on every change.

### `bb test:integration` — babashka-clone smoke suite

```sh
bb test:integration
```

Runs `test/gitsnitch/smoke/` against a real, moderately-sized git history: a
shallow clone of [babashka/babashka](https://github.com/babashka/babashka)
itself. These tests assert **structure, not values** — required keys are
present, types are correct, counts are non-negative, rows are sorted — never
exact numbers, since babashka's history isn't ours to control and will drift
over time.

The clone is cached at `.cache/gitsnitch-testing-babashka-main-clone`
(gitignored) and reused across runs — it is **never auto-updated**, so a test
run's content can't shift out from under you mid-suite. First run needs network
access to clone (shallow, ~300 commits); every run after reuses the cache and is
network-free.

If there's no cache yet and no network, the smoke tests print
`SKIP: babashka clone unavailable` and pass rather than fail.

To force a re-clone (e.g. to pick up more recent babashka history):

```sh
bb test:integration:refresh
```

### Why the CLI tests spawn a subprocess

gitsnitch has no `--repo`/`--cwd` flag — it always operates on the process's
current working directory. So `test/gitsnitch/integration/` and
`test/gitsnitch/smoke/` invoke `bb -m gitsnitch.main <args>` as a real
subprocess with its working directory set to the target repo
(`gitsnitch.fixtures/run-gitsnitch`), rather than calling `-main` in-process.

### A couple of behaviors worth knowing before writing new tests

- `--top` is applied consistently across every output format — `table`, `json`,
  and `edn` all show the same row cap for every command that supports it
  (`activity`'s rows are time buckets, not a ranked list, so it's exempt).
- `--exclude` patterns with no `/` (e.g. `*.lock`) match the basename anywhere
  in the tree, gitignore-style. A pattern containing `/` (e.g. `vendor/*`) stays
  scoped to that exact relative path.
