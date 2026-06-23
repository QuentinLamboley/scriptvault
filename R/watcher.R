#' @keywords internal
sv_watch_key <- function(project) {
  paste0("watch_", digest::digest(sv_project_root(project), algo = "sha256", serialize = FALSE))
}

#' @keywords internal
sv_fingerprints <- function(files, project) {
  if (!length(files)) {
    return(data.frame(relpath = character(), size_bytes = numeric(), modified_numeric = numeric(), stringsAsFactors = FALSE))
  }
  info <- file.info(files)
  data.frame(
    relpath = vapply(files, sv_rel_path, character(1L), project = project),
    size_bytes = as.numeric(info$size),
    modified_numeric = as.numeric(info$mtime),
    stringsAsFactors = FALSE
  )
}

#' @keywords internal
sv_changed_fingerprints <- function(previous, current) {
  if (!nrow(previous)) return(current$relpath)
  if (!nrow(current)) return(previous$relpath)
  all_paths <- union(previous$relpath, current$relpath)
  changed <- vapply(all_paths, function(path) {
    old <- previous[previous$relpath == path, , drop = FALSE]
    new <- current[current$relpath == path, , drop = FALSE]
    if (!nrow(old) || !nrow(new)) return(TRUE)
    !identical(old$size_bytes[[1L]], new$size_bytes[[1L]]) ||
      !identical(old$modified_numeric[[1L]], new$modified_numeric[[1L]])
  }, logical(1L))
  all_paths[changed]
}

#' Start automatic local archiving
#'
#' Starts a lightweight filesystem polling watcher within the current R session.
#' Once a script save reaches disk, ScriptVault detects the modified timestamp or
#' size and creates one content-addressed automatic snapshot. The watcher is
#' especially suitable for RStudio because `later` callbacks are serviced by its
#' event loop.
#'
#' A universal, cross-editor R API for every individual save keystroke does not
#' exist. The watcher archives every *detected* filesystem write at the chosen
#' interval; for an immediate explicit RStudio action, use [sv_save_active()].
#'
#' @param project Project root. Defaults to the nearest initialized vault.
#' @param interval Polling interval in seconds; one second is the default.
#' @param include_session Include session metadata on each automatic snapshot.
#' @param quiet Suppress watcher messages.
#' @return Invisibly returns the watcher status.
#' @export
sv_watch <- function(project = NULL, interval = 1, include_session = FALSE, quiet = FALSE) {
  if (!requireNamespace("later", quietly = TRUE)) {
    sv_abort("Automatic watching requires the suggested package `later`. Install it with install.packages('later').")
  }
  project <- sv_project_root(project)
  sv_assert_initialized(project)
  interval <- as.numeric(interval)
  if (is.na(interval) || interval < 0.25) sv_abort("`interval` must be at least 0.25 seconds.")

  key <- sv_watch_key(project)
  existing <- .sv_env$watchers[[key]]
  if (!is.null(existing) && isTRUE(existing$active)) {
    sv_inform("A ScriptVault watcher is already active for this project.", quiet)
    return(invisible(sv_watch_status(project, quiet = TRUE)))
  }

  files <- sv_discover_files(project)
  .sv_env$watchers[[key]] <- list(
    active = TRUE,
    project = project,
    interval = interval,
    include_session = isTRUE(include_session),
    quiet = isTRUE(quiet),
    started_at = sv_now(),
    last_tick = sv_now(),
    fingerprints = sv_fingerprints(files, project),
    snapshots_created = 0L,
    last_error = NULL
  )

  tick <- NULL
  tick <- function() {
    state <- .sv_env$watchers[[key]]
    if (is.null(state) || !isTRUE(state$active)) return(invisible(NULL))

    tryCatch({
      files <- sv_discover_files(state$project)
      current <- sv_fingerprints(files, state$project)
      changed <- sv_changed_fingerprints(state$fingerprints, current)
      state$fingerprints <- current
      state$last_tick <- sv_now()

      if (length(changed)) {
        # A full manifest keeps project-level restores deterministic, while
        # `changed` is retained in the human-readable automatic message.
        snapshot <- sv_snapshot(
          project = state$project,
          message = paste0("Automatic save detection: ", paste(utils::head(changed, 4L), collapse = ", "), if (length(changed) > 4L) " …" else ""),
          kind = "auto",
          full_manifest = TRUE,
          include_session = state$include_session,
          include_seed = FALSE,
          quiet = state$quiet
        )
        if (isTRUE(snapshot$created)) state$snapshots_created <- state$snapshots_created + 1L
      }
      state$last_error <- NULL
    }, error = function(error) {
      state$last_error <- conditionMessage(error)
      if (!isTRUE(state$quiet)) sv_warn(paste0("Automatic watcher error: ", state$last_error))
    })

    .sv_env$watchers[[key]] <- state
    if (isTRUE(state$active)) later::later(tick, delay = state$interval)
    invisible(NULL)
  }

  later::later(tick, delay = interval)
  sv_inform(paste0("Automatic local watcher started (every ", interval, " second(s))."), quiet)
  invisible(sv_watch_status(project, quiet = TRUE))
}

#' Stop automatic local archiving
#'
#' @param project Project root. Defaults to the nearest initialized vault.
#' @return Invisibly returns `TRUE`.
#' @export
sv_stop_watch <- function(project = NULL) {
  project <- sv_project_root(project)
  key <- sv_watch_key(project)
  state <- .sv_env$watchers[[key]]
  if (is.null(state) || !isTRUE(state$active)) return(invisible(TRUE))
  state$active <- FALSE
  state$stopped_at <- sv_now()
  .sv_env$watchers[[key]] <- state
  sv_inform("Automatic local watcher stopped.")
  invisible(TRUE)
}

#' Inspect automatic watcher status
#'
#' @param project Project root. Defaults to the nearest initialized vault.
#' @param quiet Suppress informational messages.
#' @return A list describing the watcher state.
#' @export
sv_watch_status <- function(project = NULL, quiet = FALSE) {
  project <- sv_project_root(project)
  key <- sv_watch_key(project)
  state <- .sv_env$watchers[[key]]
  if (is.null(state)) {
    state <- list(
      active = FALSE,
      project = project,
      interval = NA_real_,
      started_at = NA_character_,
      last_tick = NA_character_,
      snapshots_created = 0L,
      last_error = NULL
    )
  }
  if (!isTRUE(quiet)) {
    sv_inform(if (isTRUE(state$active)) "Automatic watcher is active." else "Automatic watcher is stopped.")
  }
  state
}

#' Enable automatic ScriptVault startup for an R project
#'
#' Adds a clearly delimited ScriptVault block to the project's `.Rprofile`. The
#' block starts a watcher when the project opens and only runs if `scriptvault`
#' is already installed. Existing `.Rprofile` content is preserved.
#'
#' @param project Project root. Defaults to the nearest initialized vault.
#' @param interval Polling interval in seconds.
#' @return Invisibly returns the `.Rprofile` path.
#' @export
sv_install_project_hook <- function(project = NULL, interval = 1) {
  project <- sv_project_root(project)
  sv_assert_initialized(project)
  profile <- fs::path(project, ".Rprofile")
  existing <- if (fs::file_exists(profile)) readLines(profile, warn = FALSE, encoding = "UTF-8") else character()
  start_marker <- "# >>> ScriptVault auto-watch >>>"
  end_marker <- "# <<< ScriptVault auto-watch <<<"

  start <- which(existing == start_marker)
  end <- which(existing == end_marker)
  if (length(start) && length(end)) {
    first_start <- start[[1L]]
    eligible_end <- end[end >= first_start]
    if (length(eligible_end)) {
      first_end <- eligible_end[[1L]]
      existing <- existing[-seq.int(first_start, first_end)]
    }
  }

  block <- c(
    start_marker,
    "local({",
    "  if (requireNamespace(\"scriptvault\", quietly = TRUE)) {",
    paste0("    scriptvault::sv_watch(project = getwd(), interval = ", as.numeric(interval), ", quiet = TRUE)"),
    "  }",
    "})",
    end_marker
  )
  sv_atomic_write_lines(c(existing, "", block, ""), profile)
  sv_inform("Project hook installed in `.Rprofile`. Restart the R session or run `sv_watch()` now.")
  invisible(profile)
}

#' Remove ScriptVault's project startup hook
#'
#' @param project Project root. Defaults to the nearest initialized vault.
#' @return Invisibly returns the `.Rprofile` path.
#' @export
sv_remove_project_hook <- function(project = NULL) {
  project <- sv_project_root(project)
  profile <- fs::path(project, ".Rprofile")
  if (!fs::file_exists(profile)) return(invisible(profile))
  existing <- readLines(profile, warn = FALSE, encoding = "UTF-8")
  start_marker <- "# >>> ScriptVault auto-watch >>>"
  end_marker <- "# <<< ScriptVault auto-watch <<<"
  start <- which(existing == start_marker)
  end <- which(existing == end_marker)
  if (length(start) && length(end)) {
    first_start <- start[[1L]]
    eligible_end <- end[end >= first_start]
    if (length(eligible_end)) {
      first_end <- eligible_end[[1L]]
      existing <- existing[-seq.int(first_start, first_end)]
      sv_atomic_write_lines(existing, profile)
      sv_inform("ScriptVault block removed from `.Rprofile`.")
    }
  }
  invisible(profile)
}
