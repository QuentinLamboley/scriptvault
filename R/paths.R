#' @keywords internal
sv_find_project <- function(start = getwd()) {
  current <- sv_norm_path(start)
  repeat {
    if (fs::dir_exists(fs::path(current, ".scriptvault"))) return(current)
    parent <- gsub("\\\\", "/", fs::path_dir(current), fixed = FALSE)
    if (identical(parent, current)) break
    current <- parent
  }
  sv_norm_path(start)
}

#' @keywords internal
sv_project_root <- function(project = NULL) {
  if (is.null(project)) return(sv_find_project(getwd()))
  sv_norm_path(project)
}

#' @keywords internal
sv_vault_dir <- function(project = NULL) {
  fs::path(sv_project_root(project), ".scriptvault")
}

#' @keywords internal
sv_db_path <- function(project = NULL) {
  fs::path(sv_vault_dir(project), "vault.sqlite")
}

#' @keywords internal
sv_objects_dir <- function(project = NULL) {
  fs::path(sv_vault_dir(project), "objects", "sha256")
}

#' @keywords internal
sv_recovery_dir <- function(project = NULL) {
  fs::path(sv_vault_dir(project), "recovery")
}

#' @keywords internal
sv_reports_dir <- function(project = NULL) {
  fs::path(sv_vault_dir(project), "reports")
}

#' @keywords internal
sv_locks_dir <- function(project = NULL) {
  fs::path(sv_vault_dir(project), "locks")
}

#' @keywords internal
sv_object_path <- function(hash, project = NULL) {
  fs::path(sv_objects_dir(project), paste0(hash, ".blob"))
}

#' @keywords internal
sv_is_initialized <- function(project = NULL) {
  fs::file_exists(sv_db_path(project))
}

#' @keywords internal
sv_assert_initialized <- function(project = NULL) {
  if (!sv_is_initialized(project)) {
    sv_abort("No vault was found. Run `sv_init()` inside the project first.")
  }
  invisible(TRUE)
}

#' @keywords internal
sv_create_vault_dirs <- function(project = NULL) {
  fs::dir_create(c(
    sv_vault_dir(project),
    sv_objects_dir(project),
    sv_recovery_dir(project),
    sv_reports_dir(project),
    sv_locks_dir(project)
  ), recurse = TRUE)
  invisible(sv_vault_dir(project))
}

#' @keywords internal
sv_atomic_copy <- function(from, to) {
  fs::dir_create(fs::path_dir(to), recurse = TRUE)
  if (fs::file_exists(to)) return(invisible(to))

  temporary <- paste0(to, ".partial-", sv_id("copy"))
  copied <- file.copy(from, temporary, overwrite = FALSE, copy.date = TRUE)
  if (!isTRUE(copied)) sv_abort(paste0("Unable to archive file: ", from))

  renamed <- file.rename(temporary, to)
  if (!isTRUE(renamed)) {
    copied <- file.copy(temporary, to, overwrite = FALSE, copy.date = TRUE)
    unlink(temporary, force = TRUE)
    if (!isTRUE(copied) && !fs::file_exists(to)) {
      sv_abort(paste0("Unable to finalize archive object: ", to))
    }
  }
  invisible(to)
}

#' @keywords internal
sv_atomic_write_lines <- function(lines, path) {
  fs::dir_create(fs::path_dir(path), recurse = TRUE)
  temporary <- paste0(path, ".partial-", sv_id("write"))
  writeLines(lines, temporary, useBytes = TRUE)
  if (fs::file_exists(path)) unlink(path, force = TRUE)
  if (!file.rename(temporary, path)) {
    copied <- file.copy(temporary, path, overwrite = TRUE)
    unlink(temporary, force = TRUE)
    if (!isTRUE(copied)) sv_abort(paste0("Unable to write: ", path))
  }
  invisible(path)
}
