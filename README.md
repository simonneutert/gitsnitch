# gitsnitch

A Babashka CLI for mining Git repositories. Surfaces churn hotspots, contribution concentration, commit activity, file coupling, and risky commit patterns without a database.

<div align="center">
  
  <img src="logo.png" alt="Gitsnitch Logo" width="50%" style="max-width:300px">

</div>

**Requires:** [Babashka](https://babashka.org/) >= 1.3.177

## Inspiration

- https://piechowski.io/post/git-commands-before-reading-code/
- https://oseifert.ch/blog/linux-kernel-pgit

## Installation

```sh
git clone https://github.com/simonneutert/gitsnitch
cd gitsnitch
chmod +x bin/gitsnitch
# Optionally symlink to a directory on your PATH
ln -s "$PWD/bin/gitsnitch" /usr/local/bin/gitsnitch
```

### bbin

If you have [bbin](https://github.com/babashka/bbin) installed:

```sh
bbin install io.github.simonneutert/gitsnitch
```

This installs a `gitsnitch` binary to `~/.local/bin` (ensure that directory is on your `PATH`). The `:bbin/bin` entry in `bb.edn` configures the binary name and entry point automatically — no extra flags needed.

## Usage

```sh
gitsnitch <command> [options]
```

Run inside any Git repository. All commands read from the current directory
unless `--path` restricts scope.

## Commands

### `summary`

Single-pass overview: total commits, merge ratio, top churn files, top authors,
and recent activity.

```sh
gitsnitch summary --since 6.months.ago
```

### `churn`

Files or directories ranked by change frequency.

```sh
gitsnitch churn --by file --top 20
gitsnitch churn --by dir --since 1.year.ago
gitsnitch churn --detailed          # include insertions/deletions
gitsnitch churn --exclude "*.lock"
```

| Flag         | Description                            |
| ------------ | -------------------------------------- |
| `--by`       | Aggregate by `file` (default) or `dir` |
| `--detailed` | Include insertion/deletion counts      |
| `--exclude`  | Glob pattern to exclude (repeatable)   |

### `authors`

Commit counts per author with bus-factor warnings when contribution is overly
concentrated.

```sh
gitsnitch authors --since 1.year.ago
```

### `activity`

Commit counts bucketed by time period.

```sh
gitsnitch activity --by month
gitsnitch activity --by week --since 3.months.ago
```

| Flag   | Description                                   |
| ------ | --------------------------------------------- |
| `--by` | Bucket by `month` (default), `week`, or `day` |

### `coupling`

File pairs that change together frequently (co-change analysis).

```sh
gitsnitch coupling --min-cochanges 5
```

| Flag                     | Description                                           |
| ------------------------ | ----------------------------------------------------- |
| `--min-cochanges`        | Minimum co-change count to include (default: 2)       |
| `--max-files-per-commit` | Skip commits touching more than N files (default: 50) |
| `--exclude`              | Glob pattern to exclude (repeatable)                  |

### `bugs`

Commits whose messages match bug-related keywords, ranked by file impact.

```sh
gitsnitch bugs --grep "fix|bug|broken|defect"
```

| Flag     | Description                                                                        |
| -------- | ---------------------------------------------------------------------------------- |
| `--grep` | Regex for bug keywords (default: `fix\|bug\|broken\|defect\|issue\|repair\|patch`) |

### `danger`

Commits whose messages match risk keywords (reverts, hotfixes, workarounds).

```sh
gitsnitch danger --since 6.months.ago
```

| Flag     | Description                                                                                  |
| -------- | -------------------------------------------------------------------------------------------- |
| `--grep` | Regex for danger keywords (default: `revert\|hotfix\|rollback\|emergency\|workaround\|hack`) |

## Global Options

| Flag             | Alias | Description                                                    |
| ---------------- | ----- | -------------------------------------------------------------- |
| `--rev`          | `-r`  | Git revision or range                                          |
| `--since`        |       | Lower bound on author date (e.g. `3.months.ago`, `2026-01-01`) |
| `--until`        |       | Upper bound on author date (e.g. `1.week.ago`, `2026-04-01`)  |
| `--path`         | `-p`  | Limit to files under this path prefix (repeatable)             |
| `--top`          |       | Number of results to show (default: 20)                        |
| `--limit`        | `-l`  | Maximum number of commits to process                           |
| `--format`       | `-f`  | Output format: `table`, `json`, `edn` (default: `table`)       |
| `--no-merges`    |       | Exclude merge commits (default: true)                          |
| `--first-parent` |       | Follow only first parent of merges                             |
| `--verbose`      | `-v`  | Verbose output                                                 |
| `--help`         | `-h`  | Show help                                                      |

### Date arguments

`--since` and `--until` are passed directly to `git log` without transformation,
so git's native date formats apply. The dot notation — `3.months.ago`,
`1.year.ago`, `2.weeks.ago` — is git's preferred unquoted form and avoids shell
quoting entirely. ISO dates (`2026-01-01`) and full expressions
(`"6 months ago"`) also work, but require quoting in most shells.

## Output Formats

```sh
gitsnitch churn --format json | jq .
gitsnitch summary --format edn
```

- `table` — human-readable, with progress indicator (default)
- `json` — structured, suitable for piping or scripting
- `edn` — Clojure EDN

## Development

```sh
# Run via bb tasks
bb summary
bb churn --since 1.year.ago
bb authors
bb activity --by week
bb coupling --min-cochanges 3
bb bugs
bb danger
```
