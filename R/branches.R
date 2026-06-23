#' Create a local branch
#'
#' Branches are lightweight local pointers to a snapshot. They do not copy files
#' or require Git. Switching branch changes the active history target; it does
#' not overwrite the working directory unless [sv_checkout()] is called.
#'
#' @param name New branch name using letters, numbers, `.`, `_`, `-` or `/`.
#' @param from `"HEAD"`, an existing branch, tag, or snapshot id.
#' @param description Optional branch purpose.
#' @param checkout Make the new branch active after creation.
#' @param project Project root. Defaults to the nearest initialized vault.
#' @return Invisibly returns a one-row data frame describing the new branch.
#' @export
sv_branch_create <- function(name, from = "HEAD", description = "", checkout = FALSE, project = NULL) {
  project <- sv_project_root(project)
  sv_assert_initialized(project)
  name <- as.character(name[[1L]])
  if (!grepl("^[A-Za-z0-9][A-Za-z0-9._/-]*$", name)) {
    sv_abort("Branch names must start with a letter or number and use only letters, numbers, `.`, `_`, `-` or `/`.")
  }

  row <- sv_with_lock(project, function() {
    sv_with_db(project, function(con) {
      exists <- DBI::dbGetQuery(con, "SELECT 1 FROM branches WHERE name = ?", params = list(name))
      if (nrow(exists)) sv_abort(paste0("Branch already exists: `", name, "`."))
      source_snapshot <- sv_resolve_ref_con(con, from)
      current <- sv_active_branch_con(con)

      DBI::dbBegin(con)
      committed <- FALSE
      on.exit(if (!committed) DBI::dbRollback(con), add = TRUE)
      DBI::dbExecute(
        con,
        "INSERT INTO branches(name, head_snapshot_id, created_at, created_from, description, is_active, is_archived)
         VALUES (?, ?, ?, ?, ?, 0, 0)",
        params = list(name, sv_null(source_snapshot, NA_character_), sv_now(), as.character(from), description)
      )
      if (isTRUE(checkout)) {
        DBI::dbExecute(con, "UPDATE branches SET is_active = 0")
        DBI::dbExecute(con, "UPDATE branches SET is_active = 1 WHERE name = ?", params = list(name))
        sv_set_meta(con, "active_branch", name)
      }
      sv_audit_con(
        con,
        "branch_create",
        branch = if (isTRUE(checkout)) name else current,
        snapshot_id = source_snapshot,
        details = list(name = name, from = from, checkout = isTRUE(checkout), description = description)
      )
      DBI::dbCommit(con)
      committed <- TRUE
      DBI::dbGetQuery(con, "SELECT name, head_snapshot_id, created_at, created_from, description, is_active, is_archived FROM branches WHERE name = ?", params = list(name))
    })
  })

  sv_inform(paste0("Branch `", name, "` created from ", sv_null(row$head_snapshot_id[[1L]], "an empty baseline"), "."))
  invisible(row)
}

#' List local branches
#'
#' @param project Project root. Defaults to the nearest initialized vault.
#' @return A data frame of branches and their local heads.
#' @export
sv_branch_list <- function(project = NULL, include_archived = FALSE) {
  project <- sv_project_root(project)
  sv_assert_initialized(project)
  sv_with_db(project, function(con) {
    DBI::dbGetQuery(
      con,
      paste0(
        "SELECT name, head_snapshot_id, created_at, created_from, description, is_active, is_archived ",
        "FROM branches ",
        if (isTRUE(include_archived)) "" else "WHERE is_archived = 0 ",
        "ORDER BY is_active DESC, name"
      )
    )
  })
}

#' Switch the active local branch
#'
#' @param name Existing branch name.
#' @param restore Restore the working directory to the branch head after switching.
#' @param confirm Require confirmation before restoration.
#' @param project Project root. Defaults to the nearest initialized vault.
#' @return Invisibly returns the active branch name.
#' @export
sv_branch_switch <- function(name, restore = FALSE, confirm = TRUE, project = NULL) {
  project <- sv_project_root(project)
  sv_assert_initialized(project)
  name <- as.character(name[[1L]])

  sv_with_lock(project, function() {
    sv_with_db(project, function(con) {
      exists <- DBI::dbGetQuery(con, "SELECT is_archived FROM branches WHERE name = ?", params = list(name))
      if (!nrow(exists)) sv_abort(paste0("Unknown branch: `", name, "`."))
      if (isTRUE(exists$is_archived[[1L]] == 1L)) sv_abort(paste0("Branch `", name, "` is archived and cannot be switched to."))
      previous <- sv_active_branch_con(con)
      DBI::dbBegin(con)
      committed <- FALSE
      on.exit(if (!committed) DBI::dbRollback(con), add = TRUE)
      DBI::dbExecute(con, "UPDATE branches SET is_active = 0")
      DBI::dbExecute(con, "UPDATE branches SET is_active = 1 WHERE name = ?", params = list(name))
      sv_set_meta(con, "active_branch", name)
      sv_audit_con(con, "branch_switch", branch = name, details = list(from = previous, to = name, restore = isTRUE(restore)))
      DBI::dbCommit(con)
      committed <- TRUE
    })
  })

  sv_inform(paste0("Active branch is now `", name, "`."))
  if (isTRUE(restore)) sv_restore_project(ref = name, project = project, confirm = confirm)
  invisible(name)
}

#' Archive a local branch
#'
#' @param name Branch to delete.
#' @param force Permit archiving an active branch only after switching to `main`.
#' @param project Project root. Defaults to the nearest initialized vault.
#' @return Invisibly returns `TRUE`.
#' @export
sv_branch_delete <- function(name, force = FALSE, project = NULL) {
  project <- sv_project_root(project)
  sv_assert_initialized(project)
  name <- as.character(name[[1L]])

  sv_with_lock(project, function() {
    sv_with_db(project, function(con) {
      active <- sv_active_branch_con(con)
      if (identical(name, "main")) sv_abort("The `main` branch is protected and cannot be archived.")
      if (identical(name, active) && !isTRUE(force)) {
        sv_abort("Cannot archive the active branch. Switch to another branch first, or use `force = TRUE`.")
      }
      exists <- DBI::dbGetQuery(con, "SELECT 1 FROM branches WHERE name = ?", params = list(name))
      if (!nrow(exists)) sv_abort(paste0("Unknown branch: `", name, "`."))
      if (identical(name, active)) {
        DBI::dbExecute(con, "UPDATE branches SET is_active = 0")
        DBI::dbExecute(con, "UPDATE branches SET is_active = 1 WHERE name = 'main'")
        sv_set_meta(con, "active_branch", "main")
      }
      DBI::dbExecute(con, "UPDATE branches SET is_archived = 1, is_active = 0 WHERE name = ?", params = list(name))
      sv_audit_con(con, "branch_archive", branch = sv_active_branch_con(con), details = list(name = name))
    })
  })
  sv_inform(paste0("Branch `", name, "` archived locally. Its history is retained for auditability and is hidden from the default branch list."))
  invisible(TRUE)
}

#' Reopen an archived local branch
#'
#' @param name Archived branch name.
#' @param checkout Make the reopened branch active.
#' @param project Project root. Defaults to the nearest initialized vault.
#' @return Invisibly returns the branch name.
#' @export
sv_branch_reopen <- function(name, checkout = FALSE, project = NULL) {
  project <- sv_project_root(project)
  sv_assert_initialized(project)
  name <- as.character(name[[1L]])
  sv_with_lock(project, function() {
    sv_with_db(project, function(con) {
      row <- DBI::dbGetQuery(con, "SELECT is_archived FROM branches WHERE name = ?", params = list(name))
      if (!nrow(row)) sv_abort(paste0("Unknown branch: `", name, "`."))
      DBI::dbBegin(con)
      committed <- FALSE
      on.exit(if (!committed) DBI::dbRollback(con), add = TRUE)
      DBI::dbExecute(con, "UPDATE branches SET is_archived = 0 WHERE name = ?", params = list(name))
      if (isTRUE(checkout)) {
        DBI::dbExecute(con, "UPDATE branches SET is_active = 0")
        DBI::dbExecute(con, "UPDATE branches SET is_active = 1 WHERE name = ?", params = list(name))
        sv_set_meta(con, "active_branch", name)
      }
      sv_audit_con(con, "branch_reopen", if (isTRUE(checkout)) name else sv_active_branch_con(con), details = list(name = name, checkout = isTRUE(checkout)))
      DBI::dbCommit(con)
      committed <- TRUE
    })
  })
  sv_inform(paste0("Branch `", name, "` reopened", if (isTRUE(checkout)) " and activated." else "."))
  invisible(name)
}

#' Tag a local research milestone
#'
#' @param name Tag name.
#' @param ref Snapshot reference, defaulting to `"HEAD"`.
#' @param note Optional annotation such as a manuscript submission or analysis release.
#' @param project Project root. Defaults to the nearest initialized vault.
#' @return Invisibly returns the tagged snapshot id.
#' @export
sv_tag <- function(name, ref = "HEAD", note = "", project = NULL) {
  project <- sv_project_root(project)
  sv_assert_initialized(project)
  name <- as.character(name[[1L]])
  if (!grepl("^[A-Za-z0-9][A-Za-z0-9._/-]*$", name)) sv_abort("Invalid tag name.")

  snapshot <- sv_with_lock(project, function() {
    sv_with_db(project, function(con) {
      snapshot <- sv_resolve_ref_con(con, ref)
      if (is.null(snapshot)) sv_abort("Cannot tag an empty branch.")
      DBI::dbExecute(
        con,
        "INSERT INTO tags(name, snapshot_id, created_at, note) VALUES (?, ?, ?, ?)
         ON CONFLICT(name) DO UPDATE SET snapshot_id = excluded.snapshot_id, created_at = excluded.created_at, note = excluded.note",
        params = list(name, snapshot, sv_now(), note)
      )
      sv_audit_con(con, "tag", sv_active_branch_con(con), snapshot, list(name = name, note = note))
      snapshot
    })
  })
  sv_inform(paste0("Tag `", name, "` now points to ", snapshot, "."))
  invisible(snapshot)
}

#' List local tags
#'
#' @param project Project root. Defaults to the nearest initialized vault.
#' @return A data frame of local tags.
#' @export
sv_tag_list <- function(project = NULL) {
  project <- sv_project_root(project)
  sv_assert_initialized(project)
  sv_with_db(project, function(con) {
    DBI::dbGetQuery(con, "SELECT name, snapshot_id, created_at, note FROM tags ORDER BY created_at DESC")
  })
}
