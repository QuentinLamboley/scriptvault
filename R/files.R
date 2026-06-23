#' @keywords internal
sv_default_ignored_prefixes <- function() {
  c(
    ".scriptvault/", ".git/", ".Rproj.user/", "renv/library/",
    "renv/staging/", "node_modules/", "_book/", "_site/"
  )
}

#' @keywords internal
sv_ignore_file <- function(project = NULL) {
  fs::path(sv_project_root(project), ".scriptvaultignore")
}

#' @keywords internal
sv_user_ignore_patterns <- function(project = NULL) {
  path <- sv_ignore_file(project)
  if (!fs::file_exists(path)) return(character())
  lines <- readLines(path, warn = FALSE, encoding = "UTF-8")
  lines <- trimws(lines)
  lines[nzchar(lines) & !startsWith(lines, "#")]
}

#' @keywords internal
sv_should_ignore <- function(relpath, project = NULL) {
  relpath <- gsub("\\\\", "/", relpath, fixed = FALSE)
  if (startsWith(relpath, ".scriptvault/")) return(TRUE)
  if (any(startsWith(relpath, sv_default_ignored_prefixes()))) return(TRUE)
  if (basename(relpath) %in% c(".Rhistory", ".RData", ".Ruserdata")) return(TRUE)

  patterns <- sv_user_ignore_patterns(project)
  if (!length(patterns)) return(FALSE)
  any(vapply(patterns, function(pattern) {
    isTRUE(tryCatch(grepl(pattern, relpath, perl = TRUE), error = function(e) FALSE))
  }, logical(1L)))
}

#' @keywords internal
sv_tracked_files_con <- function(con) {
  out <- DBI::dbGetQuery(con, "SELECT relpath FROM tracked_files WHERE active = 1 ORDER BY relpath")
  if (!nrow(out)) character() else as.character(out$relpath)
}

#' @keywords internal
sv_is_default_script <- function(path, extensions = sv_default_extensions()) {
  filename <- basename(path)
  extension <- tools::file_ext(filename)
  filename %in% c(".Rprofile", ".Renviron") || extension %in% extensions
}

#' @keywords internal
sv_discover_files <- function(project = NULL, paths = NULL, extensions = sv_default_extensions()) {
  project <- sv_project_root(project)
  tracked <- character()
  if (sv_is_initialized(project)) {
    tracked <- sv_with_db(project, function(con) sv_tracked_files_con(con))
  }

  if (is.null(paths)) {
    candidates <- fs::dir_ls(project, recurse = TRUE, type = "file", fail = FALSE)
  } else {
    candidates <- sv_as_project_path(paths, project)
    candidates <- candidates[fs::file_exists(candidates)]
  }

  if (length(tracked)) {
    tracked_paths <- sv_as_project_path(tracked, project)
    candidates <- unique(c(candidates, tracked_paths[fs::file_exists(tracked_paths)]))
  }
  if (!length(candidates)) return(character())

  candidates <- sv_norm_path(candidates)
  relative <- vapply(candidates, sv_rel_path, character(1L), project = project)
  keep <- !vapply(relative, sv_should_ignore, logical(1L), project = project)
  candidates <- candidates[keep]
  relative <- relative[keep]
  if (!length(candidates)) return(character())

  tracked_keep <- relative %in% tracked
  default_keep <- vapply(candidates, sv_is_default_script, logical(1L), extensions = extensions)
  unique(candidates[tracked_keep | default_keep])
}

#' Track an additional file in ScriptVault
#'
#' Adds a file outside the default script extensions to the project manifest.
#' The file remains local and is archived only when a snapshot is created.
#'
#' @param paths Paths relative to the project or absolute paths inside it.
#' @param project Project root. Defaults to the nearest initialized vault.
#' @return Invisibly returns the tracked relative paths.
#' @export
sv_track <- function(paths, project = NULL) {
  project <- sv_project_root(project)
  sv_assert_initialized(project)
  absolute <- sv_as_project_path(paths, project)
  if (!all(fs::file_exists(absolute))) sv_abort("Every file supplied to `sv_track()` must exist.")
  relative <- vapply(absolute, sv_rel_path, character(1L), project = project)
  if (any(startsWith(relative, "../"))) sv_abort("Tracked files must be inside the project root.")

  sv_with_lock(project, function() {
    sv_with_db(project, function(con) {
      DBI::dbBegin(con)
      committed <- FALSE
      on.exit(if (!committed) DBI::dbRollback(con), add = TRUE)
      for (path in relative) {
        DBI::dbExecute(
          con,
          "INSERT INTO tracked_files(relpath, added_at, active) VALUES (?, ?, 1)
           ON CONFLICT(relpath) DO UPDATE SET active = 1",
          params = list(path, sv_now())
        )
      }
      sv_audit_con(con, "track", sv_active_branch_con(con), details = list(paths = relative))
      DBI::dbCommit(con)
      committed <- TRUE
    })
  })

  sv_inform(paste0(length(relative), " file(s) added to the tracked manifest."))
  invisible(relative)
}

#' Stop tracking files in ScriptVault
#'
#' @param paths Paths relative to the project or absolute paths inside it.
#' @param project Project root. Defaults to the nearest initialized vault.
#' @return Invisibly returns the untracked relative paths.
#' @export
sv_untrack <- function(paths, project = NULL) {
  project <- sv_project_root(project)
  sv_assert_initialized(project)
  relative <- vapply(sv_as_project_path(paths, project), sv_rel_path, character(1L), project = project)

  sv_with_lock(project, function() {
    sv_with_db(project, function(con) {
      for (path in relative) {
        DBI::dbExecute(con, "UPDATE tracked_files SET active = 0 WHERE relpath = ?", params = list(path))
      }
      sv_audit_con(con, "untrack", sv_active_branch_con(con), details = list(paths = relative))
    })
  })

  sv_inform(paste0(length(relative), " file(s) removed from explicit tracking."))
  invisible(relative)
}

#' @keywords internal
sv_manifest_rows <- function(project, files, previous_manifest) {
  current <- list()
  if (length(files)) {
    for (path in files) {
      relative <- sv_rel_path(path, project)
      info <- sv_file_info(path)
      hash <- sv_hash_file(path)
      current[[relative]] <- list(
        relpath = relative,
        object_hash = hash,
        size_bytes = info$size_bytes[[1L]],
        modified_at = info$modified_at[[1L]],
        is_deleted = 0L
      )
    }
  }

  previous_paths <- if (nrow(previous_manifest)) previous_manifest$relpath else character()
  current_paths <- names(current)
  missing <- setdiff(previous_paths, current_paths)
  if (length(missing)) {
    for (relative in missing) {
      previous <- previous_manifest[previous_manifest$relpath == relative, , drop = FALSE][1L, , drop = FALSE]
      current[[relative]] <- list(
        relpath = relative,
        object_hash = as.character(previous$object_hash[[1L]]),
        size_bytes = as.numeric(previous$size_bytes[[1L]]),
        modified_at = if (previous$is_deleted[[1L]] == 1L) as.character(previous$modified_at[[1L]]) else sv_now(),
        is_deleted = 1L
      )
    }
  }

  if (!length(current)) {
    return(data.frame(
      relpath = character(), object_hash = character(), size_bytes = numeric(),
      modified_at = character(), is_deleted = integer(), changed = integer(),
      stringsAsFactors = FALSE
    ))
  }

  rows <- do.call(rbind, lapply(current, function(x) {
    data.frame(
      relpath = x$relpath,
      object_hash = x$object_hash,
      size_bytes = x$size_bytes,
      modified_at = x$modified_at,
      is_deleted = as.integer(x$is_deleted),
      stringsAsFactors = FALSE
    )
  }))
  rows <- rows[order(rows$relpath), , drop = FALSE]

  rows$changed <- vapply(seq_len(nrow(rows)), function(index) {
    row <- rows[index, , drop = FALSE]
    previous <- previous_manifest[previous_manifest$relpath == row$relpath, , drop = FALSE]
    if (!nrow(previous)) return(1L)
    changed <- !identical(as.character(row$object_hash[[1L]]), as.character(previous$object_hash[[1L]])) ||
      !identical(as.integer(row$is_deleted[[1L]]), as.integer(previous$is_deleted[[1L]]))
    as.integer(changed)
  }, integer(1L))

  rows
}

#' @keywords internal
sv_manifest_hash <- function(rows) {
  if (!nrow(rows)) return(sv_hash_text(character()))
  lines <- paste(rows$relpath, rows$object_hash, rows$is_deleted, sep = "\t")
  sv_hash_text(lines)
}

#' @keywords internal
sv_working_changes_con <- function(con, project) {
  head <- sv_branch_head_con(con, sv_active_branch_con(con))
  previous <- sv_manifest_con(con, head)
  files <- sv_discover_files(project)
  current <- sv_manifest_rows(project, files, previous)

  if (!nrow(current) && !nrow(previous)) {
    return(data.frame(relpath = character(), status = character(), stringsAsFactors = FALSE))
  }

  output <- character(nrow(current))
  for (index in seq_len(nrow(current))) {
    row <- current[index, , drop = FALSE]
    previous_row <- previous[previous$relpath == row$relpath, , drop = FALSE]
    if (!nrow(previous_row)) {
      output[[index]] <- if (row$is_deleted[[1L]] == 1L) "unchanged" else "new"
    } else if (row$is_deleted[[1L]] == 1L && previous_row$is_deleted[[1L]] == 0L) {
      output[[index]] <- "deleted"
    } else if (row$is_deleted[[1L]] == 0L && previous_row$is_deleted[[1L]] == 1L) {
      output[[index]] <- "restored"
    } else if (!identical(as.character(row$object_hash[[1L]]), as.character(previous_row$object_hash[[1L]]))) {
      output[[index]] <- "modified"
    } else {
      output[[index]] <- "unchanged"
    }
  }
  data.frame(relpath = current$relpath, status = output, stringsAsFactors = FALSE)
}
