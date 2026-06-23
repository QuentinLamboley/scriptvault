.sv_env <- new.env(parent = emptyenv())
.sv_env$watchers <- list()
.sv_env$package_version <- "0.1.0"

#' ScriptVault's default file extensions
#'
#' These extensions cover common R workflows as well as closely related
#' reproducibility assets. Additional files can be added explicitly with
#' [sv_track()].
#' @keywords internal
sv_default_extensions <- function() {
  c(
    "R", "r", "Rmd", "rmd", "qmd", "Rnw", "rnw", "Rprofile",
    "py", "jl", "stan", "do", "sh", "bash", "zsh",
    "yaml", "yml", "json", "toml", "ini", "cfg", "txt"
  )
}

#' @keywords internal
sv_now <- function() {
  format(Sys.time(), tz = "UTC", usetz = TRUE)
}

#' @keywords internal
sv_abort <- function(message, call. = FALSE) {
  stop(paste0("ScriptVault: ", message), call. = call.)
}

#' @keywords internal
sv_warn <- function(message) {
  warning(paste0("ScriptVault: ", message), call. = FALSE, immediate. = TRUE)
}

#' @keywords internal
sv_inform <- function(message, quiet = FALSE) {
  if (!isTRUE(quiet)) {
    message(paste0("ScriptVault | ", message))
  }
  invisible(message)
}

#' @keywords internal
sv_null <- function(x, y) {
  if (is.null(x) || length(x) == 0L || is.na(x)[1L]) y else x
}

#' @keywords internal
sv_id <- function(prefix = "sv") {
  entropy <- paste(
    prefix,
    format(Sys.time(), "%Y%m%d%H%M%OS6", tz = "UTC"),
    Sys.getpid(),
    runif(1L),
    sep = "-"
  )
  paste0(prefix, "_", substr(digest::digest(entropy, algo = "sha256", serialize = FALSE), 1L, 20L))
}

#' @keywords internal
sv_is_absolute <- function(path) {
  grepl("^(?:[A-Za-z]:[\\\\/]|/|\\\\\\\\)", path, perl = TRUE)
}

#' @keywords internal
sv_norm_path <- function(path) {
  gsub("\\\\", "/", fs::path_norm(fs::path_abs(path)), fixed = FALSE)
}

#' @keywords internal
sv_rel_path <- function(path, project) {
  gsub("\\\\", "/", fs::path_rel(sv_norm_path(path), start = sv_norm_path(project)), fixed = FALSE)
}

#' @keywords internal
sv_as_project_path <- function(paths, project) {
  paths <- as.character(paths)
  vapply(paths, function(path) {
    if (sv_is_absolute(path)) sv_norm_path(path) else sv_norm_path(fs::path(project, path))
  }, character(1L), USE.NAMES = FALSE)
}

#' @keywords internal
sv_hash_file <- function(path) {
  digest::digest(file = path, algo = "sha256", serialize = FALSE)
}

#' @keywords internal
sv_hash_text <- function(text) {
  digest::digest(paste(text, collapse = "\n"), algo = "sha256", serialize = FALSE)
}

#' @keywords internal
sv_json <- function(x, pretty = FALSE) {
  jsonlite::toJSON(x, auto_unbox = TRUE, null = "null", pretty = pretty, na = "null")
}

#' @keywords internal
sv_from_json <- function(x, default = NULL) {
  if (is.null(x) || length(x) == 0L || is.na(x)[1L] || !nzchar(x[1L])) return(default)
  tryCatch(jsonlite::fromJSON(x, simplifyVector = FALSE), error = function(e) default)
}

#' @keywords internal
sv_file_info <- function(path) {
  info <- file.info(path)
  data.frame(
    path = sv_norm_path(path),
    size_bytes = as.numeric(info$size),
    modified_at = format(info$mtime, tz = "UTC", usetz = TRUE),
    stringsAsFactors = FALSE
  )
}

#' @keywords internal
sv_session_metadata <- function(include_seed = TRUE) {
  seed <- NULL
  if (isTRUE(include_seed) && exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)) {
    seed <- as.integer(get(".Random.seed", envir = .GlobalEnv, inherits = FALSE))
  }

  list(
    captured_at = sv_now(),
    r_version = R.version.string,
    platform = R.version$platform,
    locale = tryCatch(Sys.getlocale(), error = function(e) NA_character_),
    timezone = tryCatch(Sys.timezone(), error = function(e) NA_character_),
    rng_kind = RNGkind(),
    random_seed = seed,
    session_info = capture.output(utils::sessionInfo())
  )
}

#' @keywords internal
sv_confirm <- function(prompt, confirm = TRUE) {
  if (!isTRUE(confirm)) return(TRUE)
  if (!interactive()) {
    sv_abort(paste0(prompt, " Set `confirm = FALSE` when calling non-interactively."))
  }
  answer <- tolower(trimws(readline(paste0(prompt, " [y/N] "))))
  answer %in% c("y", "yes", "o", "oui")
}
