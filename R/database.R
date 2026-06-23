#' @keywords internal
sv_db_connect <- function(project = NULL) {
  sv_assert_initialized(project)
  con <- DBI::dbConnect(RSQLite::SQLite(), dbname = sv_db_path(project))
  DBI::dbExecute(con, "PRAGMA foreign_keys = ON")
  DBI::dbExecute(con, "PRAGMA busy_timeout = 10000")
  con
}

#' @keywords internal
sv_with_db <- function(project = NULL, fun) {
  con <- sv_db_connect(project)
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  fun(con)
}

#' @keywords internal
sv_with_lock <- function(project = NULL, fun, timeout = 15) {
  project <- sv_project_root(project)
  sv_create_vault_dirs(project)
  lock_path <- fs::path(sv_locks_dir(project), "vault.lock")
  lock <- filelock::lock(lock_path, timeout = as.integer(timeout * 1000))
  if (is.null(lock)) {
    sv_abort("The vault is currently locked by another ScriptVault operation. Try again in a moment.")
  }
  on.exit(filelock::unlock(lock), add = TRUE)
  fun()
}

#' @keywords internal
sv_db_initialize <- function(project) {
  db_path <- sv_db_path(project)
  con <- DBI::dbConnect(RSQLite::SQLite(), dbname = db_path)
  on.exit(DBI::dbDisconnect(con), add = TRUE)

  DBI::dbExecute(con, "PRAGMA foreign_keys = ON")
  DBI::dbExecute(con, "PRAGMA journal_mode = WAL")
  DBI::dbExecute(con, "PRAGMA busy_timeout = 10000")

  statements <- c(
    "CREATE TABLE IF NOT EXISTS meta (
       key TEXT PRIMARY KEY,
       value TEXT NOT NULL
     )",
    "CREATE TABLE IF NOT EXISTS branches (
       name TEXT PRIMARY KEY,
       head_snapshot_id TEXT,
       created_at TEXT NOT NULL,
       created_from TEXT,
       description TEXT,
       is_active INTEGER NOT NULL DEFAULT 0,
       is_archived INTEGER NOT NULL DEFAULT 0,
       FOREIGN KEY(head_snapshot_id) REFERENCES snapshots(id)
     )",
    "CREATE TABLE IF NOT EXISTS snapshots (
       id TEXT PRIMARY KEY,
       parent_id TEXT,
       branch TEXT NOT NULL,
       created_at TEXT NOT NULL,
       kind TEXT NOT NULL,
       message TEXT,
       author TEXT,
       manifest_hash TEXT NOT NULL,
       r_version TEXT,
       platform TEXT,
       session_json TEXT,
       seed_json TEXT,
       file_count INTEGER NOT NULL,
       changed_count INTEGER NOT NULL,
       FOREIGN KEY(parent_id) REFERENCES snapshots(id),
       FOREIGN KEY(branch) REFERENCES branches(name)
     )",
    "CREATE TABLE IF NOT EXISTS files (
       snapshot_id TEXT NOT NULL,
       relpath TEXT NOT NULL,
       object_hash TEXT,
       size_bytes REAL,
       modified_at TEXT,
       is_deleted INTEGER NOT NULL DEFAULT 0,
       changed INTEGER NOT NULL DEFAULT 0,
       PRIMARY KEY(snapshot_id, relpath),
       FOREIGN KEY(snapshot_id) REFERENCES snapshots(id) ON DELETE CASCADE
     )",
    "CREATE TABLE IF NOT EXISTS tracked_files (
       relpath TEXT PRIMARY KEY,
       added_at TEXT NOT NULL,
       active INTEGER NOT NULL DEFAULT 1
     )",
    "CREATE TABLE IF NOT EXISTS tags (
       name TEXT PRIMARY KEY,
       snapshot_id TEXT NOT NULL,
       created_at TEXT NOT NULL,
       note TEXT,
       FOREIGN KEY(snapshot_id) REFERENCES snapshots(id)
     )",
    "CREATE TABLE IF NOT EXISTS runs (
       id TEXT PRIMARY KEY,
       snapshot_id TEXT,
       branch TEXT,
       started_at TEXT NOT NULL,
       command TEXT,
       parameters_json TEXT,
       environment_json TEXT,
       note TEXT,
       FOREIGN KEY(snapshot_id) REFERENCES snapshots(id)
     )",
    "CREATE TABLE IF NOT EXISTS artifacts (
       id TEXT PRIMARY KEY,
       snapshot_id TEXT,
       run_id TEXT,
       relpath TEXT NOT NULL,
       role TEXT NOT NULL,
       hash TEXT NOT NULL,
       size_bytes REAL,
       copied_object_hash TEXT,
       metadata_json TEXT,
       created_at TEXT NOT NULL,
       FOREIGN KEY(snapshot_id) REFERENCES snapshots(id),
       FOREIGN KEY(run_id) REFERENCES runs(id)
     )",
    "CREATE TABLE IF NOT EXISTS audit (
       id TEXT PRIMARY KEY,
       created_at TEXT NOT NULL,
       action TEXT NOT NULL,
       actor TEXT,
       branch TEXT,
       snapshot_id TEXT,
       details_json TEXT,
       FOREIGN KEY(snapshot_id) REFERENCES snapshots(id)
     )",
    "CREATE INDEX IF NOT EXISTS idx_snapshots_branch_time ON snapshots(branch, created_at DESC)",
    "CREATE INDEX IF NOT EXISTS idx_files_relpath ON files(relpath)",
    "CREATE INDEX IF NOT EXISTS idx_artifacts_snapshot ON artifacts(snapshot_id)",
    "CREATE INDEX IF NOT EXISTS idx_audit_time ON audit(created_at DESC)"
  )

  for (statement in statements) DBI::dbExecute(con, statement)
  # Forward-compatible no-op on new vaults; applies the soft-delete field to
  # early development vaults created before branch archiving was introduced.
  try(DBI::dbExecute(con, "ALTER TABLE branches ADD COLUMN is_archived INTEGER NOT NULL DEFAULT 0"), silent = TRUE)

  sv_set_meta(con, "schema_version", "1")
  sv_set_meta(con, "package_version", .sv_env$package_version)
  sv_set_meta(con, "project_root", sv_project_root(project))
  sv_set_meta(con, "project_id", sv_null(sv_get_meta(con, "project_id"), sv_id("project")))
  sv_set_meta(con, "created_at", sv_null(sv_get_meta(con, "created_at"), sv_now()))

  existing_main <- DBI::dbGetQuery(con, "SELECT name FROM branches WHERE name = 'main'")
  if (nrow(existing_main) == 0L) {
    DBI::dbExecute(
      con,
      "INSERT INTO branches(name, head_snapshot_id, created_at, created_from, description, is_active)
       VALUES (?, NULL, ?, NULL, ?, 1)",
      params = list("main", sv_now(), "Primary local branch")
    )
  }
  sv_set_meta(con, "active_branch", sv_null(sv_get_meta(con, "active_branch"), "main"))

  invisible(TRUE)
}

#' @keywords internal
sv_get_meta <- function(con, key, default = NULL) {
  values <- DBI::dbGetQuery(con, "SELECT value FROM meta WHERE key = ?", params = list(key))
  if (nrow(values) == 0L) return(default)
  values$value[[1L]]
}

#' @keywords internal
sv_set_meta <- function(con, key, value) {
  DBI::dbExecute(
    con,
    "INSERT INTO meta(key, value) VALUES (?, ?)
     ON CONFLICT(key) DO UPDATE SET value = excluded.value",
    params = list(as.character(key), as.character(value))
  )
  invisible(value)
}

#' @keywords internal
sv_active_branch_con <- function(con) {
  sv_get_meta(con, "active_branch", "main")
}

#' @keywords internal
sv_branch_head_con <- function(con, branch) {
  out <- DBI::dbGetQuery(con, "SELECT head_snapshot_id FROM branches WHERE name = ?", params = list(branch))
  if (nrow(out) == 0L) sv_abort(paste0("Unknown branch: `", branch, "`."))
  value <- out$head_snapshot_id[[1L]]
  if (is.na(value) || !nzchar(value)) NULL else value
}

#' @keywords internal
sv_manifest_con <- function(con, snapshot_id) {
  if (is.null(snapshot_id) || !nzchar(snapshot_id)) {
    return(data.frame(
      relpath = character(), object_hash = character(), size_bytes = numeric(),
      modified_at = character(), is_deleted = integer(), changed = integer(),
      stringsAsFactors = FALSE
    ))
  }
  out <- DBI::dbGetQuery(
    con,
    "SELECT relpath, object_hash, size_bytes, modified_at, is_deleted, changed
     FROM files WHERE snapshot_id = ? ORDER BY relpath",
    params = list(snapshot_id)
  )
  out$relpath <- as.character(out$relpath)
  out
}

#' @keywords internal
sv_snapshot_exists_con <- function(con, snapshot_id) {
  out <- DBI::dbGetQuery(con, "SELECT 1 FROM snapshots WHERE id = ? LIMIT 1", params = list(snapshot_id))
  nrow(out) == 1L
}

#' @keywords internal
sv_resolve_ref_con <- function(con, ref = "HEAD") {
  if (is.null(ref) || !length(ref) || is.na(ref[[1L]]) || !nzchar(ref[[1L]])) {
    ref <- "HEAD"
  }
  ref <- as.character(ref[[1L]])
  active_branch <- sv_active_branch_con(con)

  if (identical(ref, "HEAD")) return(sv_branch_head_con(con, active_branch))

  branch <- DBI::dbGetQuery(con, "SELECT head_snapshot_id FROM branches WHERE name = ?", params = list(ref))
  if (nrow(branch) == 1L) {
    value <- branch$head_snapshot_id[[1L]]
    return(if (is.na(value) || !nzchar(value)) NULL else value)
  }

  tag <- DBI::dbGetQuery(con, "SELECT snapshot_id FROM tags WHERE name = ?", params = list(ref))
  if (nrow(tag) == 1L) return(tag$snapshot_id[[1L]])

  if (sv_snapshot_exists_con(con, ref)) return(ref)
  sv_abort(paste0("Cannot resolve reference `", ref, "`. Use HEAD, a branch, a tag or a snapshot id."))
}

#' @keywords internal
sv_audit_con <- function(con, action, branch = NULL, snapshot_id = NULL, details = list(), actor = Sys.info()[["user"]]) {
  DBI::dbExecute(
    con,
    "INSERT INTO audit(id, created_at, action, actor, branch, snapshot_id, details_json)
     VALUES (?, ?, ?, ?, ?, ?, ?)",
    params = list(
      sv_id("audit"), sv_now(), action,
      sv_null(actor, NA_character_), sv_null(branch, NA_character_),
      sv_null(snapshot_id, NA_character_), sv_json(details)
    )
  )
  invisible(TRUE)
}
