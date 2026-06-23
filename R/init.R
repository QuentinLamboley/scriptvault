#' Initialize a local ScriptVault project
#'
#' Creates a `.scriptvault` directory in the selected project. The directory
#' contains the local SQLite index, content-addressed script objects, recovery
#' copies and reproducibility reports. No remote service, Git repository or
#' account is created.
#'
#' @param project Project root. Defaults to the current working directory.
#' @param snapshot_initial Should existing scripts be archived immediately?
#' @param author Optional default author stored in vault metadata.
#' @param quiet Suppress informative messages.
#' @return An object of class `sv_status`.
#' @export
sv_init <- function(project = getwd(), snapshot_initial = TRUE, author = Sys.info()[["user"]], quiet = FALSE) {
  project <- sv_project_root(project)
  if (!fs::dir_exists(project)) sv_abort(paste0("Project directory does not exist: ", project))

  sv_create_vault_dirs(project)
  created <- !sv_is_initialized(project)
  sv_with_lock(project, function() sv_db_initialize(project))

  sv_with_lock(project, function() {
    sv_with_db(project, function(con) {
      if (!is.null(author) && length(author) && nzchar(author[[1L]])) {
        sv_set_meta(con, "default_author", as.character(author[[1L]]))
      }
      sv_audit_con(
        con,
        action = if (created) "init" else "reopen",
        branch = sv_active_branch_con(con),
        details = list(project = project, package_version = .sv_env$package_version)
      )
    })
  })

  if (created) {
    ignore_path <- sv_ignore_file(project)
    if (!fs::file_exists(ignore_path)) {
      sv_atomic_write_lines(c(
        "# ScriptVault ignore rules: one regular expression per line.",
        "# Examples: ^data/raw/   |   \\.csv$   |   ^outputs/"
      ), ignore_path)
    }
    sv_inform(paste0("Vault initialized in ", fs::path(project, ".scriptvault")), quiet)
  } else {
    sv_inform("Existing local vault opened.", quiet)
  }

  if (isTRUE(snapshot_initial) && created) {
    existing <- sv_discover_files(project)
    if (length(existing)) {
      sv_snapshot(
        project = project,
        message = "Initial project baseline",
        kind = "checkpoint",
        force = TRUE,
        quiet = quiet
      )
    } else {
      sv_inform("No scripts were found for the initial baseline; the vault is ready.", quiet)
    }
  }

  sv_status(project)
}

#' Show the current local versioning status
#'
#' @param project Project root. Defaults to the nearest initialized vault.
#' @return An object of class `sv_status` containing a compact project summary.
#' @export
sv_status <- function(project = NULL) {
  project <- sv_project_root(project)
  sv_assert_initialized(project)

  result <- sv_with_db(project, function(con) {
    branch <- sv_active_branch_con(con)
    head <- sv_branch_head_con(con, branch)
    branches <- DBI::dbGetQuery(con, "SELECT COUNT(*) AS n FROM branches WHERE is_archived = 0")$n[[1L]]
    snapshots <- DBI::dbGetQuery(con, "SELECT COUNT(*) AS n FROM snapshots")$n[[1L]]
    files <- sv_discover_files(project)
    changes <- sv_working_changes_con(con, project)
    changes <- changes[changes$status != "unchanged", , drop = FALSE]
    list(
      project = project,
      vault = sv_vault_dir(project),
      branch = branch,
      head = head,
      branch_count = as.integer(branches),
      snapshot_count = as.integer(snapshots),
      tracked_script_count = length(files),
      pending_changes = changes,
      auto_watcher = sv_watch_status(project, quiet = TRUE)
    )
  })
  class(result) <- "sv_status"
  result
}

#' @export
print.sv_status <- function(x, ...) {
  cat("\n")
  cat("  ScriptVault status\n")
  cat("  ──────────────────\n")
  cat("  Project : ", x$project, "\n", sep = "")
  cat("  Branch  : ", x$branch, "\n", sep = "")
  cat("  Head    : ", sv_null(x$head, "<no snapshot yet>"), "\n", sep = "")
  cat("  History : ", x$snapshot_count, " snapshot(s) across ", x$branch_count, " branch(es)\n", sep = "")
  cat("  Scripts : ", x$tracked_script_count, " currently discoverable\n", sep = "")
  cat("  Watcher : ", if (isTRUE(x$auto_watcher$active)) "active" else "stopped", "\n", sep = "")
  if (nrow(x$pending_changes)) {
    cat("  Changes : ", nrow(x$pending_changes), " pending\n", sep = "")
    for (index in seq_len(min(nrow(x$pending_changes), 8L))) {
      cat("            ", sprintf("%-9s", x$pending_changes$status[[index]]), x$pending_changes$relpath[[index]], "\n", sep = "")
    }
    if (nrow(x$pending_changes) > 8L) cat("            …\n")
  } else {
    cat("  Changes : working tree matches the current local snapshot\n")
  }
  cat("\n")
  invisible(x)
}
