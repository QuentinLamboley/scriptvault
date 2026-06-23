#' @keywords internal
sv_backup_path <- function(project, relative, label = "restore") {
  stamp <- gsub("[^0-9A-Za-z]", "", format(Sys.time(), "%Y%m%dT%H%M%OS6", tz = "UTC"))
  fs::path(sv_recovery_dir(project), paste0(stamp, "-", label), relative)
}

#' @keywords internal
sv_backup_file <- function(project, relative, label = "restore") {
  source <- fs::path(project, relative)
  if (!fs::file_exists(source)) return(NULL)
  destination <- sv_backup_path(project, relative, label)
  fs::dir_create(fs::path_dir(destination), recurse = TRUE)
  copied <- file.copy(source, destination, overwrite = FALSE, copy.date = TRUE)
  if (!isTRUE(copied)) sv_abort(paste0("Unable to create recovery copy for `", relative, "`."))
  destination
}

#' @keywords internal
sv_restore_object <- function(project, hash, relative) {
  object <- sv_object_path(hash, project)
  if (!fs::file_exists(object)) sv_abort(paste0("Cannot restore `", relative, "`: archived object is missing."))
  target <- fs::path(project, relative)
  fs::dir_create(fs::path_dir(target), recurse = TRUE)
  temporary <- paste0(target, ".scriptvault-restore-", sv_id("tmp"))
  copied <- file.copy(object, temporary, overwrite = TRUE, copy.date = TRUE)
  if (!isTRUE(copied)) sv_abort(paste0("Unable to stage restored file: `", relative, "`."))
  if (fs::file_exists(target)) unlink(target, force = TRUE)
  moved <- file.rename(temporary, target)
  if (!isTRUE(moved)) {
    copied <- file.copy(temporary, target, overwrite = TRUE, copy.date = TRUE)
    unlink(temporary, force = TRUE)
    if (!isTRUE(copied)) sv_abort(paste0("Unable to restore `", relative, "`."))
  }
  invisible(target)
}

#' Restore one script from local history
#'
#' The current file is copied to `.scriptvault/recovery/` by default before it is
#' overwritten. Restoration itself is immediately committed as a local `restore`
#' snapshot, so the operation is reversible.
#'
#' @param path Script to restore.
#' @param ref Branch, tag or snapshot id to restore from.
#' @param backup Create a recovery copy of the current file first.
#' @param project Project root. Defaults to the nearest initialized vault.
#' @param quiet Suppress informative messages.
#' @return An `sv_snapshot` object documenting the restoration.
#' @export
sv_restore <- function(path, ref = "HEAD", backup = TRUE, project = NULL, quiet = FALSE) {
  project <- sv_project_root(project)
  sv_assert_initialized(project)
  relative <- sv_rel_path(sv_as_project_path(path, project)[[1L]], project)

  restored <- sv_with_lock(project, function() {
    sv_with_db(project, function(con) {
      snapshot <- sv_resolve_ref_con(con, ref)
      if (is.null(snapshot)) sv_abort("The selected reference does not have a snapshot.")
      row <- sv_manifest_file_con(con, snapshot, relative)
      if (is.null(row) || row$is_deleted[[1L]] == 1L) {
        sv_abort(paste0("`", relative, "` does not exist in reference `", ref, "`."))
      }
      if (isTRUE(backup)) sv_backup_file(project, relative, label = "single-file-restore")
      sv_restore_object(project, row$object_hash[[1L]], relative)
      sv_audit_con(con, "restore_file_staged", sv_active_branch_con(con), snapshot, list(path = relative, ref = ref, backup = isTRUE(backup)))
      invisible(TRUE)
    })
  })
  invisible(restored)

  sv_snapshot(
    project = project,
    message = paste0("Restored `", relative, "` from `", ref, "`"),
    kind = "restore",
    force = TRUE,
    quiet = quiet
  )
}

#' Restore a whole project manifest from local history
#'
#' This is a deliberate operation. By default ScriptVault backs up every
#' currently discoverable script before writing the target manifest and requires
#' interactive confirmation. `prune = TRUE` additionally removes currently
#' tracked scripts absent from the selected manifest.
#'
#' @param ref Branch, tag or snapshot id to restore from.
#' @param backup Create recovery copies before writing or deleting files.
#' @param prune Remove current discoverable scripts absent from the target manifest.
#' @param confirm Ask for confirmation. Set to `FALSE` only in controlled scripts.
#' @param project Project root. Defaults to the nearest initialized vault.
#' @param quiet Suppress informative messages.
#' @return An `sv_snapshot` object documenting the restoration.
#' @export
sv_restore_project <- function(ref = "HEAD", backup = TRUE, prune = FALSE, confirm = TRUE, project = NULL, quiet = FALSE) {
  project <- sv_project_root(project)
  sv_assert_initialized(project)

  if (!sv_confirm(paste0("Restore the project manifest from `", ref, "`? Existing scripts may be overwritten."), confirm)) {
    sv_inform("Project restoration cancelled.", quiet)
    return(invisible(NULL))
  }

  summary <- sv_with_lock(project, function() {
    sv_with_db(project, function(con) {
      snapshot <- sv_resolve_ref_con(con, ref)
      if (is.null(snapshot)) sv_abort("The selected reference does not have a snapshot.")
      manifest <- sv_manifest_con(con, snapshot)
      target_live <- manifest$relpath[manifest$is_deleted == 0L]
      current <- sv_discover_files(project)
      current_relative <- if (length(current)) vapply(current, sv_rel_path, character(1L), project = project) else character()

      changed <- character()
      for (index in seq_len(nrow(manifest))) {
        row <- manifest[index, , drop = FALSE]
        relative <- row$relpath[[1L]]
        target <- fs::path(project, relative)

        if (row$is_deleted[[1L]] == 1L) {
          if (fs::file_exists(target) && isTRUE(prune)) {
            if (isTRUE(backup)) sv_backup_file(project, relative, label = "project-restore")
            unlink(target, force = TRUE)
            changed <- c(changed, relative)
          }
          next
        }

        current_hash <- if (fs::file_exists(target)) sv_hash_file(target) else NULL
        if (is.null(current_hash) || !identical(current_hash, row$object_hash[[1L]])) {
          if (isTRUE(backup)) sv_backup_file(project, relative, label = "project-restore")
          sv_restore_object(project, row$object_hash[[1L]], relative)
          changed <- c(changed, relative)
        }
      }

      if (isTRUE(prune)) {
        extras <- setdiff(current_relative, target_live)
        for (relative in extras) {
          target <- fs::path(project, relative)
          if (fs::file_exists(target)) {
            if (isTRUE(backup)) sv_backup_file(project, relative, label = "project-prune")
            unlink(target, force = TRUE)
            changed <- c(changed, relative)
          }
        }
      }

      sv_audit_con(
        con,
        "restore_project_staged",
        sv_active_branch_con(con),
        snapshot,
        list(ref = ref, backup = isTRUE(backup), prune = isTRUE(prune), changed = unique(changed))
      )
      list(snapshot = snapshot, changed = unique(changed))
    })
  })

  sv_snapshot(
    project = project,
    message = paste0("Restored project manifest from `", ref, "` (", length(summary$changed), " file operation(s))"),
    kind = "restore",
    force = TRUE,
    quiet = quiet
  )
}

#' Check out a local branch, tag or snapshot
#'
#' @param ref Local branch, tag or snapshot id.
#' @param confirm Ask for confirmation before writing files.
#' @param project Project root. Defaults to the nearest initialized vault.
#' @return An `sv_snapshot` object documenting the restoration.
#' @export
sv_checkout <- function(ref, confirm = TRUE, project = NULL) {
  project <- sv_project_root(project)
  sv_assert_initialized(project)
  branches <- sv_branch_list(project)
  if (ref %in% branches$name) {
    sv_branch_switch(ref, restore = FALSE, project = project)
  }
  sv_restore_project(ref = ref, project = project, confirm = confirm)
}
