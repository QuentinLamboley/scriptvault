#' Create a local ScriptVault snapshot
#'
#' A snapshot writes only previously unseen file content into the local vault.
#' Repeated content is de-duplicated through SHA-256 object names. The SQLite
#' index stores a complete manifest for each snapshot, which makes branch-level
#' restoration deterministic and auditable.
#'
#' @param paths Optional paths to prioritize. With `full_manifest = TRUE`, all
#'   discovered project scripts still become part of the manifest.
#' @param project Project root. Defaults to the nearest initialized vault.
#' @param message Human-readable reason for the snapshot.
#' @param kind One of `"manual"`, `"auto"`, `"checkpoint"` or `"restore"`.
#' @param branch Branch receiving the snapshot. Defaults to the active branch.
#' @param full_manifest Store every discovered script in the manifest.
#' @param include_session Include R and session metadata for reproducibility.
#' @param include_seed Include the current R random seed when available.
#' @param force Record a snapshot even when content has not changed.
#' @param quiet Suppress informative messages.
#' @return An object of class `sv_snapshot`.
#' @export
sv_snapshot <- function(
    paths = NULL,
    project = NULL,
    message = NULL,
    kind = c("manual", "auto", "checkpoint", "restore"),
    branch = NULL,
    full_manifest = TRUE,
    include_session = TRUE,
    include_seed = TRUE,
    force = FALSE,
    quiet = FALSE) {
  kind <- match.arg(kind)
  project <- sv_project_root(project)
  sv_assert_initialized(project)

  result <- sv_with_lock(project, function() {
    sv_with_db(project, function(con) {
      active <- sv_active_branch_con(con)
      branch <- sv_null(branch, active)
      branch <- as.character(branch[[1L]])
      head <- sv_branch_head_con(con, branch)
      previous <- sv_manifest_con(con, head)

      files <- if (isTRUE(full_manifest) || is.null(paths)) {
        sv_discover_files(project)
      } else {
        sv_discover_files(project, paths = paths)
      }

      if (!length(files) && !nrow(previous)) {
        sv_abort("No eligible scripts were found. Add a file with `sv_track()` or change the project contents.")
      }

      manifest <- sv_manifest_rows(project, files, previous)
      changed_count <- sum(manifest$changed)
      if (!isTRUE(force) && identical(changed_count, 0L)) {
        return(structure(list(
          created = FALSE,
          id = head,
          branch = branch,
          changed_count = 0L,
          file_count = nrow(manifest),
          message = "No content change detected; no redundant snapshot created."
        ), class = "sv_snapshot"))
      }

      # Archive new content before indexing it. Orphaned objects after a rare
      # interrupted transaction are harmless and can be removed with sv_gc().
      live_rows <- manifest[manifest$is_deleted == 0L, , drop = FALSE]
      if (nrow(live_rows)) {
        for (index in seq_len(nrow(live_rows))) {
          relative <- live_rows$relpath[[index]]
          source <- fs::path(project, relative)
          object <- sv_object_path(live_rows$object_hash[[index]], project)
          if (!fs::file_exists(object)) sv_atomic_copy(source, object)
        }
      }

      snapshot_id <- sv_id("snap")
      default_author <- sv_get_meta(con, "default_author", Sys.info()[["user"]])
      metadata <- if (isTRUE(include_session)) sv_session_metadata(include_seed) else list()
      session_json <- if (isTRUE(include_session)) sv_json(metadata) else NA_character_
      seed_json <- if (isTRUE(include_session) && isTRUE(include_seed)) sv_json(metadata$random_seed) else NA_character_
      message <- sv_null(message, switch(
        kind,
        auto = "Automatic filesystem snapshot",
        checkpoint = "Research checkpoint",
        restore = "Restoration checkpoint",
        manual = "Manual local snapshot"
      ))

      DBI::dbBegin(con)
      committed <- FALSE
      on.exit(if (!committed) DBI::dbRollback(con), add = TRUE)

      DBI::dbExecute(
        con,
        "INSERT INTO snapshots(
           id, parent_id, branch, created_at, kind, message, author,
           manifest_hash, r_version, platform, session_json, seed_json,
           file_count, changed_count
         ) VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)",
        params = list(
          snapshot_id,
          sv_null(head, NA_character_),
          branch,
          sv_now(),
          kind,
          as.character(message),
          sv_null(default_author, NA_character_),
          sv_manifest_hash(manifest),
          sv_null(metadata$r_version, NA_character_),
          sv_null(metadata$platform, NA_character_),
          session_json,
          seed_json,
          as.integer(nrow(manifest)),
          as.integer(changed_count)
        )
      )

      if (nrow(manifest)) {
        for (index in seq_len(nrow(manifest))) {
          row <- manifest[index, , drop = FALSE]
          DBI::dbExecute(
            con,
            "INSERT INTO files(snapshot_id, relpath, object_hash, size_bytes, modified_at, is_deleted, changed)
             VALUES (?, ?, ?, ?, ?, ?, ?)",
            params = list(
              snapshot_id,
              row$relpath[[1L]],
              row$object_hash[[1L]],
              as.numeric(row$size_bytes[[1L]]),
              row$modified_at[[1L]],
              as.integer(row$is_deleted[[1L]]),
              as.integer(row$changed[[1L]])
            )
          )
        }
      }

      DBI::dbExecute(
        con,
        "UPDATE branches SET head_snapshot_id = ? WHERE name = ?",
        params = list(snapshot_id, branch)
      )
      sv_audit_con(
        con,
        "snapshot",
        branch = branch,
        snapshot_id = snapshot_id,
        details = list(
          kind = kind,
          message = message,
          file_count = nrow(manifest),
          changed_count = changed_count,
          full_manifest = isTRUE(full_manifest)
        )
      )
      DBI::dbCommit(con)
      committed <- TRUE

      structure(list(
        created = TRUE,
        id = snapshot_id,
        parent_id = head,
        branch = branch,
        kind = kind,
        message = message,
        changed_count = as.integer(changed_count),
        file_count = nrow(manifest),
        created_at = sv_now()
      ), class = "sv_snapshot")
    })
  })

  if (isTRUE(result$created)) {
    sv_inform(
      paste0(
        "Snapshot ", result$id,
        " archived on branch `", result$branch, "` (",
        result$changed_count, " changed file", if (result$changed_count == 1L) "", "s", ")."
      ),
      quiet = quiet
    )
  } else {
    sv_inform(result$message, quiet = quiet)
  }
  result
}

#' @export
print.sv_snapshot <- function(x, ...) {
  if (!isTRUE(x$created)) {
    cat("ScriptVault snapshot: no new version created\n")
    cat("Reason: ", x$message, "\n", sep = "")
    return(invisible(x))
  }
  cat("ScriptVault snapshot\n")
  cat("  id      : ", x$id, "\n", sep = "")
  cat("  branch  : ", x$branch, "\n", sep = "")
  cat("  kind    : ", x$kind, "\n", sep = "")
  cat("  changed : ", x$changed_count, " file(s)\n", sep = "")
  cat("  message : ", x$message, "\n", sep = "")
  invisible(x)
}
