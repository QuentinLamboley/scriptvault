#' @keywords internal
sv_active_rstudio_path <- function() {
  if (!requireNamespace("rstudioapi", quietly = TRUE) || !rstudioapi::isAvailable()) {
    sv_abort("This action requires an active RStudio session and the optional `rstudioapi` package.")
  }
  context <- rstudioapi::getActiveDocumentContext()
  path <- context$path
  if (is.null(path) || !nzchar(path)) {
    sv_abort("The active RStudio document has not been saved to disk yet.")
  }
  list(path = sv_norm_path(path), id = context$id)
}

#' Snapshot the active RStudio document
#'
#' @param message Optional snapshot message.
#' @param project Project root. Defaults to the nearest initialized vault.
#' @return An `sv_snapshot` object.
#' @export
sv_snapshot_active <- function(message = NULL, project = NULL) {
  document <- sv_active_rstudio_path()
  sv_snapshot(
    paths = document$path,
    project = project,
    message = sv_null(message, paste0("RStudio active document snapshot: ", basename(document$path))),
    kind = "manual"
  )
}

#' Save and immediately snapshot the active RStudio document
#'
#' This function is designed for the ScriptVault RStudio addin and is the most
#' direct way to pair one explicit RStudio save command with one local archive.
#'
#' @param message Optional snapshot message.
#' @param project Project root. Defaults to the nearest initialized vault.
#' @return An `sv_snapshot` object.
#' @export
sv_save_active <- function(message = NULL, project = NULL) {
  document <- sv_active_rstudio_path()
  rstudioapi::documentSave(id = document$id)
  Sys.sleep(0.08)
  sv_snapshot(
    paths = document$path,
    project = project,
    message = sv_null(message, paste0("Saved and archived from RStudio: ", basename(document$path))),
    kind = "manual"
  )
}

#' RStudio addin: snapshot active script
#' @export
sv_addin_snapshot_active <- function() {
  sv_snapshot_active()
  invisible(NULL)
}

#' RStudio addin: save and snapshot active script
#' @export
sv_addin_save_and_snapshot <- function() {
  sv_save_active()
  invisible(NULL)
}

#' RStudio addin: start automatic watcher
#' @export
sv_addin_start_watcher <- function() {
  sv_watch()
  invisible(NULL)
}

#' RStudio addin: stop automatic watcher
#' @export
sv_addin_stop_watcher <- function() {
  sv_stop_watch()
  invisible(NULL)
}
