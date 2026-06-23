\name{scriptvault-package}
\alias{scriptvault-package}
\alias{sv_init}
\alias{sv_status}
\alias{sv_snapshot}
\alias{sv_history}
\alias{sv_file_history}
\alias{sv_diff}
\alias{sv_restore}
\alias{sv_restore_project}
\alias{sv_branch_create}
\alias{sv_branch_list}
\alias{sv_branch_switch}
\alias{sv_branch_delete}
\alias{sv_branch_reopen}
\alias{sv_tag}
\alias{sv_tag_list}
\alias{sv_checkout}
\alias{sv_track}
\alias{sv_untrack}
\alias{sv_watch}
\alias{sv_stop_watch}
\alias{sv_watch_status}
\alias{sv_install_project_hook}
\alias{sv_remove_project_hook}
\alias{sv_snapshot_active}
\alias{sv_save_active}
\alias{sv_addin_snapshot_active}
\alias{sv_addin_save_and_snapshot}
\alias{sv_addin_start_watcher}
\alias{sv_addin_stop_watcher}
\alias{sv_record_run}
\alias{sv_artifact_register}
\alias{sv_reproducibility_report}
\alias{sv_export_manifest}
\alias{sv_verify_integrity}
\alias{sv_gc}
\title{ScriptVault: local-first versioning for research scripts}
\description{
ScriptVault archives research scripts locally without Git, GitHub or a remote service.
It provides content-addressed snapshots, logical branches, local tags, history,
diffs, safe restoration, optional automatic filesystem watching, reproducibility
run records, artifact fingerprints and integrity validation.
}
\usage{
sv_init(project = getwd(), snapshot_initial = TRUE)
sv_snapshot(message = NULL, kind = "manual")
sv_watch(interval = 1)
sv_branch_create(name, from = "HEAD")
sv_history()
sv_diff(path)
sv_restore(path, ref = "HEAD")
sv_record_run(command = NULL, parameters = list())
}
\details{
A vault is created in \file{.scriptvault} at the project root. It contains a local
SQLite index and deduplicated SHA-256 archive objects. ScriptVault does not call a
network service and does not require Git to be installed.

For automatic snapshots, \code{sv_watch()} checks files written to disk during the
current R session. The optional RStudio addin \code{sv_save_active()} provides a
direct save-and-archive command for the currently active document.
}
\value{
Functions return structured lists or data frames appropriate to their operation.
Snapshot-related functions return objects with class \code{sv_snapshot}; history
returns \code{sv_history}; \code{sv_status()} returns \code{sv_status}.
}
\examples{
\dontrun{
sv_init()
sv_watch()
sv_snapshot(message = "Before model calibration")
sv_branch_create("sensitivity-resolution")
sv_record_run(
  command = "R/04_fit_models.R",
  parameters = list(resolution_km = 5, folds = 5)
)
sv_reproducibility_report()
}
}
