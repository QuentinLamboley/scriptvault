#' Inspect local snapshot history
#'
#' @param path Optional project-relative file path. When supplied, only snapshots
#'   that changed this file are returned by default.
#' @param branch Optional branch filter. Defaults to the active branch.
#' @param limit Maximum number of rows.
#' @param include_unchanged For a file history, include snapshots where the file
#'   was carried forward unchanged in a complete manifest.
#' @param project Project root. Defaults to the nearest initialized vault.
#' @return An object of class `sv_history` wrapping a data frame.
#' @export
sv_history <- function(path = NULL, branch = NULL, limit = 50L, include_unchanged = FALSE, project = NULL) {
  project <- sv_project_root(project)
  sv_assert_initialized(project)
  limit <- as.integer(limit)
  if (is.na(limit) || limit < 1L) sv_abort("`limit` must be a positive integer.")

  selected_branch <- sv_with_db(project, function(con) sv_null(branch, sv_active_branch_con(con)))
  data <- sv_with_db(project, function(con) {
    if (is.null(path)) {
      DBI::dbGetQuery(
        con,
        "SELECT id, parent_id, branch, created_at, kind, message, author, file_count, changed_count
         FROM snapshots WHERE branch = ? ORDER BY created_at DESC LIMIT ?",
        params = list(selected_branch, limit)
      )
    } else {
      relative <- sv_rel_path(sv_as_project_path(path, project)[[1L]], project)
      changed_filter <- if (isTRUE(include_unchanged)) "" else " AND f.changed = 1"
      DBI::dbGetQuery(
        con,
        paste0(
          "SELECT s.id, s.parent_id, s.branch, s.created_at, s.kind, s.message, s.author,
                  f.relpath, f.object_hash, f.is_deleted, f.changed
           FROM snapshots s INNER JOIN files f ON s.id = f.snapshot_id
           WHERE s.branch = ? AND f.relpath = ?", changed_filter,
          " ORDER BY s.created_at DESC LIMIT ?"
        ),
        params = list(selected_branch, relative, limit)
      )
    }
  })

  result <- list(data = data, path = path, branch = selected_branch)
  class(result) <- "sv_history"
  result
}

#' File-focused history shortcut
#'
#' @inheritParams sv_history
#' @return An object of class `sv_history`.
#' @export
sv_file_history <- function(path, branch = NULL, limit = 50L, include_unchanged = FALSE, project = NULL) {
  sv_history(path = path, branch = branch, limit = limit, include_unchanged = include_unchanged, project = project)
}

#' @export
print.sv_history <- function(x, ...) {
  if (!nrow(x$data)) {
    cat("ScriptVault history is empty for this selection.\n")
    return(invisible(x))
  }
  label <- if (is.null(x$path)) paste0("branch `", x$branch, "`") else paste0("file `", x$path, "`")
  cat("\nScriptVault history for ", label, "\n", sep = "")
  cat("────────────────────────────────────────────────────────────────\n")
  for (index in seq_len(nrow(x$data))) {
    row <- x$data[index, , drop = FALSE]
    cat(
      sprintf(
        "%2d. %-24s  %-10s  %s\n",
        index,
        substr(row$id[[1L]], 1L, 24L),
        row$kind[[1L]],
        row$created_at[[1L]]
      )
    )
    cat("    ", row$message[[1L]], "\n", sep = "")
  }
  cat("\n")
  invisible(x)
}

#' @keywords internal
sv_blob_lines <- function(project, hash) {
  path <- sv_object_path(hash, project)
  if (!fs::file_exists(path)) sv_abort(paste0("Vault object is missing or damaged: ", hash))
  readLines(path, warn = FALSE, encoding = "UTF-8")
}

#' @keywords internal
sv_manifest_file_con <- function(con, snapshot_id, relpath) {
  out <- DBI::dbGetQuery(
    con,
    "SELECT relpath, object_hash, size_bytes, modified_at, is_deleted, changed
     FROM files WHERE snapshot_id = ? AND relpath = ?",
    params = list(snapshot_id, relpath)
  )
  if (!nrow(out)) NULL else out[1L, , drop = FALSE]
}

#' @keywords internal
sv_lcs_diff <- function(before, after, max_cells = 2500000L) {
  n <- length(before)
  m <- length(after)
  if (as.double(n) * as.double(m) > max_cells) {
    return(c(
      "--- before", "+++ after",
      paste0("@@ comparison abbreviated: ", n, " vs ", m, " lines; use smaller files for a full line diff @@"),
      paste0("- lines: ", n),
      paste0("+ lines: ", m)
    ))
  }

  table <- matrix(0L, nrow = n + 1L, ncol = m + 1L)
  if (n && m) {
    for (i in seq_len(n)) {
      for (j in seq_len(m)) {
        if (identical(before[[i]], after[[j]])) {
          table[i + 1L, j + 1L] <- table[i, j] + 1L
        } else {
          table[i + 1L, j + 1L] <- max(table[i, j + 1L], table[i + 1L, j])
        }
      }
    }
  }

  operations <- list()
  i <- n
  j <- m
  while (i > 0L || j > 0L) {
    if (i > 0L && j > 0L && identical(before[[i]], after[[j]])) {
      operations[[length(operations) + 1L]] <- paste0(" ", before[[i]])
      i <- i - 1L
      j <- j - 1L
    } else if (j > 0L && (i == 0L || table[i + 1L, j] >= table[i, j + 1L])) {
      operations[[length(operations) + 1L]] <- paste0("+", after[[j]])
      j <- j - 1L
    } else {
      operations[[length(operations) + 1L]] <- paste0("-", before[[i]])
      i <- i - 1L
    }
  }
  c("--- before", "+++ after", rev(unlist(operations, use.names = FALSE)))
}

#' Compare two local versions of a script
#'
#' @param path Path to the script to compare.
#' @param from Baseline reference; defaults to the parent of `to` where possible.
#' @param to Target reference, defaulting to `"HEAD"`.
#' @param project Project root. Defaults to the nearest initialized vault.
#' @return An object of class `sv_diff`.
#' @export
sv_diff <- function(path, from = NULL, to = "HEAD", project = NULL) {
  project <- sv_project_root(project)
  sv_assert_initialized(project)
  relative <- sv_rel_path(sv_as_project_path(path, project)[[1L]], project)

  result <- sv_with_db(project, function(con) {
    to_id <- sv_resolve_ref_con(con, to)
    if (is.null(to_id)) sv_abort("The target reference has no snapshot yet.")
    if (is.null(from)) {
      parent <- DBI::dbGetQuery(con, "SELECT parent_id FROM snapshots WHERE id = ?", params = list(to_id))$parent_id[[1L]]
      from_id <- if (is.na(parent) || !nzchar(parent)) NULL else parent
    } else {
      from_id <- sv_resolve_ref_con(con, from)
    }

    left <- if (is.null(from_id)) NULL else sv_manifest_file_con(con, from_id, relative)
    right <- sv_manifest_file_con(con, to_id, relative)
    before <- if (is.null(left) || left$is_deleted[[1L]] == 1L) character() else sv_blob_lines(project, left$object_hash[[1L]])
    after <- if (is.null(right) || right$is_deleted[[1L]] == 1L) character() else sv_blob_lines(project, right$object_hash[[1L]])
    list(
      path = relative,
      from = from_id,
      to = to_id,
      lines = sv_lcs_diff(before, after),
      changed = !identical(before, after)
    )
  })
  class(result) <- "sv_diff"
  result
}

#' @export
print.sv_diff <- function(x, ...) {
  cat("\nScriptVault diff: ", x$path, "\n", sep = "")
  cat("from: ", sv_null(x$from, "<empty baseline>"), "\n", sep = "")
  cat("to:   ", x$to, "\n", sep = "")
  cat(paste(x$lines, collapse = "\n"), "\n", sep = "")
  invisible(x)
}
