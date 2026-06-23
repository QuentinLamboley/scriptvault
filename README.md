# ScriptVault <img src="https://img.shields.io/badge/local--first-no%20cloud-1f6feb" align="right" height="24" />

**A premium local versioning vault for R research projects — without Git, GitHub, accounts or remote storage.**

ScriptVault keeps a complete, auditable history of scripts written in a research project. It is built for researchers who need the benefits of versioning and reproducibility without learning Git commands or connecting their work to an online repository.

> **Privacy by design:** ScriptVault does not communicate with a network service. The entire history lives in a project-local `.scriptvault/` directory.

---

## What it does

| Need | ScriptVault response |
|---|---|
| Every meaningful script save | Optional local watcher detects the filesystem write and archives a content-addressed version. |
| Separate exploratory analyses | Create lightweight **branches** locally: no Git and no duplicated project copy. |
| Understand what changed | Browse history and compare two versions with a readable line-level diff. |
| Undo safely | Restore a single script or a project manifest; existing files are copied to a recovery folder first. |
| Preserve research context | Capture R session information, random seed, parameters, inputs, outputs, models and figures. |
| Share a review trail | Generate a local Markdown reproducibility report and JSON manifest. |
| Detect corruption | Verify hashes of all archived objects and inspect removable orphan objects. |

### Design principles

- **Local first:** no cloud, telemetry, credentials, repository or account.
- **Content-addressed:** each unique file content is stored once under its SHA-256 hash.
- **Research-friendly:** branch names can represent hypotheses, sensitivity analyses, manuscript milestones or reviewer responses.
- **Safe by default:** restoration keeps recovery copies; exact project pruning is opt-in.
- **Simple interface:** usable through a handful of R functions or RStudio addins.

---

## Install from GitHub

Once the repository has been published, install the development version directly from GitHub:

```r
install.packages("remotes")
remotes::install_github("QuentinLamboley/scriptvault")
library(scriptvault)
```

Replace `VOTRE_COMPTE_GITHUB` with the owner of the GitHub repository. The package itself remains fully local-first after installation: ScriptVault does not require Git, GitHub, a cloud account, or a network connection to archive a project.

## Install from this release folder

### 1. Install dependencies once

```r
install.packages(c("DBI", "RSQLite", "digest", "filelock", "fs", "jsonlite", "later", "rstudioapi"))
```

### 2. Install the packaged source archive

From the directory containing `scriptvault_0.1.0.tar.gz`:

```r
install.packages("scriptvault_0.1.0.tar.gz", repos = NULL, type = "source")
```

Then load it:

```r
library(scriptvault)
```

No Git, Rtools or compilation toolchain is required by ScriptVault itself: it contains only R code. Standard CRAN binaries are generally used for its dependencies on Windows and macOS.

---

## Five-minute start

Open the R project you want to protect and run:

```r
library(scriptvault)

# Creates .scriptvault/ and records a baseline of current scripts
sv_init()

# Starts automatic local archiving during this R session
sv_watch(interval = 1)

# Inspect branch, last snapshot and files not yet archived
sv_status()
```

After a save reaches disk, the watcher creates a local snapshot at the next polling cycle. The default is **one second**.

To make automatic protection start whenever the project opens in RStudio:

```r
sv_install_project_hook(interval = 1)
```

This only adds a clearly delimited ScriptVault block to the project `.Rprofile`; it does not alter files elsewhere.

---

## Essential workflow for a scientific project

```r
library(scriptvault)

sv_init()
sv_watch()

# Before a major methodological choice
sv_snapshot(message = "Before environmental pseudo-absence sensitivity analysis")

# Explore an alternative without losing the baseline
sv_branch_create(
  "sensitivity-10km",
  description = "Resolution sensitivity analysis at 10 km",
  checkout = TRUE
)

# Work, save scripts normally, then tag a defensible milestone
sv_tag("analysis-v1", note = "Model set frozen for internal review")

# Record the execution context of an analysis
sv_record_run(
  command = "R/05_fit_sdm.R",
  parameters = list(
    resolution_km = 10,
    pa_ratio = "5:1",
    pseudoabsence_replicates = 5,
    folds = 5
  ),
  inputs = c("data/occurrences.csv", "data/predictors.csv"),
  outputs = c("outputs/model_metrics.csv", "outputs/suitability.tif"),
  copy_artifacts = FALSE,
  note = "Candidate resolution-sensitivity run"
)

# Produce a report ready to archive beside a manuscript or supplement
sv_reproducibility_report()
```

---

## RStudio addins

After installing the package, restart RStudio. In **Addins**, ScriptVault provides:

- **Snapshot active script** — archive the document currently open in the source editor.
- **Save and snapshot active script** — directly pair a deliberate RStudio save with a local snapshot.
- **Start ScriptVault watcher** / **Stop ScriptVault watcher** — control the automatic local watcher.

### Important implementation note

R does not expose a universal cross-editor event that fires for every `Ctrl/Cmd+S` action. ScriptVault therefore offers two complementary modes:

1. `sv_watch()` detects files saved to disk during the active R session, at a configurable interval (one second by default). It is the practical hands-free mode for RStudio projects.
2. The **Save and snapshot active script** addin calls RStudio’s save API and archives immediately afterwards. Bind it to a personal keyboard shortcut when a one-command save-and-archive action is preferred.

The watcher does not run once R is closed. `sv_install_project_hook()` starts it automatically next time the R project opens.

---

## Core commands

### Versioning

```r
sv_snapshot(message = "Before refactoring the model selection function")
sv_history()
sv_file_history("R/05_fit_sdm.R")
sv_diff("R/05_fit_sdm.R")
```

### Branches and tags

```r
sv_branch_create("reviewer-2-response", from = "analysis-v1", checkout = TRUE)
sv_branch_list()
sv_branch_switch("main")          # does not overwrite files
sv_checkout("analysis-v1")         # switches branch if relevant and restores after confirmation
sv_branch_delete("reviewer-2-response")  # archives it locally; history remains auditable
sv_branch_reopen("reviewer-2-response")  # makes an archived branch visible again
sv_tag("submission-2026-07-01", note = "Version sent to journal")
sv_tag_list()
```

### Safe restoration

```r
# One script, with a recovery copy first
sv_restore("R/05_fit_sdm.R", ref = "analysis-v1")

# Whole tracked project manifest; confirmation is required interactively
sv_restore_project(ref = "analysis-v1")

# Exact restoration, including removal of scripts not in that version
sv_restore_project(ref = "analysis-v1", prune = TRUE)
```

### Reproducibility and integrity

```r
sv_artifact_register(
  "outputs/final_map.tif",
  role = "figure",
  metadata = list(description = "Final habitat-suitability map")
)

sv_record_run(
  command = "R/07_make_figures.R",
  parameters = list(dpi = 600, palette = "viridis")
)

sv_reproducibility_report()
sv_export_manifest()
sv_verify_integrity()
sv_gc()                    # dry run: reports only
# sv_gc(dry_run = FALSE)   # removes truly unreferenced archive objects
```

---

## What is stored in `.scriptvault/`?

```text
.scriptvault/
├── vault.sqlite             # local index: branches, snapshots, tags, audit, runs
├── objects/
│   └── sha256/              # deduplicated complete file content
├── recovery/                # pre-restoration safety copies
├── reports/                 # reproducibility reports and JSON manifests
└── locks/                   # protects concurrent writes
```

Back up **the project and `.scriptvault/` together**. Excluding `.scriptvault/` excludes the local history.

---

## Default scope and custom files

By default, ScriptVault discovers common research code and configuration files: `.R`, `.Rmd`, `.qmd`, `.Rnw`, `.py`, `.jl`, `.stan`, shell scripts, and YAML/JSON/TOML/text configuration files.

To include a non-standard file:

```r
sv_track("config/private-analysis-settings.xml")
```

To stop explicitly tracking it:

```r
sv_untrack("config/private-analysis-settings.xml")
```

Create `.scriptvaultignore` at project root to add regular-expression exclusions:

```text
^data/raw/
^outputs/
\\.csv$
```

Large raw datasets are fingerprinted with `sv_artifact_register()` by default rather than automatically copied. This preserves traceability without silently duplicating terabytes of data.

---

## Safety and limitations

- ScriptVault archives complete contents of tracked files. Treat `.scriptvault/` as sensitive if code or paths are sensitive.
- Full-project restore is intentionally conservative. It creates recovery copies by default and asks for confirmation in an interactive session.
- Branch switching does **not** overwrite your working directory. Use `sv_checkout()` or `sv_restore_project()` when you intentionally want to restore files.
- The watcher records detected filesystem changes. Very rapid consecutive saves within one polling interval can be coalesced into one snapshot containing the latest saved state. Use the **Save and snapshot** addin for a direct, explicit save-plus-archive operation.
- Version 0.1.0 is a first operational release. It is designed for local research workflows and should be backed up before being relied on as the sole copy of an important project.

---

## Suggested research conventions

Use branches to express scientific decisions, not technical jargon:

```text
main
sensitivity-2km
sensitivity-10km
pseudoabsence-5to1
reviewer-1-response
manuscript-submission
```

Use tags for defendable milestones:

```text
baseline-validated
analysis-frozen
figures-final
submission-v1
revision-1-resubmission
```

Pair each key analysis execution with `sv_record_run()`, including parameters, inputs and outputs. It turns local versioning into a reproducibility ledger rather than a simple file backup.
