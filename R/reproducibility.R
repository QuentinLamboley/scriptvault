#' @keywords internal
sv_register_artifact_con <- function(con, path, role, snapshot_id, run_id = NULL, copy = FALSE, metadata = list(), project) {
  absolute <- sv_as_project_path(path, project)[[1L]]
  if (!fs::file_exists(absolute)) sv_abort(paste0("Artifact does not exist: ", absolute))
  relative <- sv_rel_path(absolute, project)
  info <- sv_file_info(absolute)
  hash <- sv_hash_file(absolute)
  copied_hash <- NA_character_
  if (isTRUE(copy)) {
    object <- sv_object_path(hash, project)
    if (!fs::file_exists(object)) sv_atomic_copy(absolute, object)
    copied_hash <- hash
  }

  artifact_id <- sv_id("artifact")
  DBI::dbExecute(
    con,
    "INSERT INTO artifacts(id, snapshot_id, run_id, relpath, role, hash, size_bytes, copied_object_hash, metadata_json, created_at)
     VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?)",
    params = list(
      artifact_id,
      sv_null(snapshot_id, NA_character_),
      sv_null(run_id, NA_character_),
      relative,
      role,
      hash,
      info$size_bytes[[1L]],
      copied_hash,
      sv_json(metadata),
      sv_now()
    )
  )
  list(id = artifact_id, relpath = relative, hash = hash, copied = isTRUE(copy))
}

#' Register a research artifact with a local checksum
#'
#' This function records a SHA-256 fingerprint for an input, output, model,
#' figure or data file. By default it records metadata only, avoiding duplicate
#' storage of potentially large datasets. Set `copy = TRUE` to archive the exact
#' artifact bytes inside the local vault.
#'
#' @param path File path to register.
#' @param role One of `"input"`, `"output"`, `"figure"`, `"model"`, `"data"`
#'   or a custom role label.
#' @param snapshot Snapshot reference, defaulting to `"HEAD"`.
#' @param copy Archive a content-addressed copy of the artifact.
#' @param metadata Optional named list, such as a DOI, source URL or processing note.
#' @param project Project root. Defaults to the nearest initialized vault.
#' @return A list describing the registered artifact.
#' @export
sv_artifact_register <- function(path, role = "data", snapshot = "HEAD", copy = FALSE, metadata = list(), project = NULL) {
  project <- sv_project_root(project)
  sv_assert_initialized(project)
  role <- as.character(role[[1L]])
  if (!nzchar(role)) sv_abort("`role` must be a non-empty label.")

  artifact <- sv_with_lock(project, function() {
    sv_with_db(project, function(con) {
      snapshot_id <- sv_resolve_ref_con(con, snapshot)
      artifact <- sv_register_artifact_con(con, path, role, snapshot_id, copy = copy, metadata = metadata, project = project)
      sv_audit_con(
        con,
        "artifact_register",
        sv_active_branch_con(con),
        snapshot_id,
        details = list(path = artifact$relpath, role = role, copied = isTRUE(copy), hash = artifact$hash)
      )
      artifact
    })
  })
  sv_inform(paste0("Artifact fingerprint recorded for `", artifact$relpath, "`."))
  artifact
}

#' Record a reproducible analysis run
#'
#' Creates a local run record linked to a ScriptVault snapshot. The record stores
#' the invoked command, parameters, R session metadata and optional checksums of
#' input and output artifacts. It is intended to support manuscripts, peer review,
#' model tuning and later forensic reconstruction of an analysis.
#'
#' @param command Human-readable command, script, pipeline target or analysis label.
#' @param parameters Named list of analysis parameters.
#' @param inputs Optional vector of input file paths.
#' @param outputs Optional vector of output file paths.
#' @param snapshot Snapshot reference, defaulting to `"HEAD"`.
#' @param note Optional research note.
#' @param copy_artifacts Archive the exact bytes of all listed artifacts.
#' @param project Project root. Defaults to the nearest initialized vault.
#' @return A list containing the run id and registered artifacts.
#' @export
sv_record_run <- function(
    command = NULL,
    parameters = list(),
    inputs = NULL,
    outputs = NULL,
    snapshot = "HEAD",
    note = "",
    copy_artifacts = FALSE,
    project = NULL) {
  project <- sv_project_root(project)
  sv_assert_initialized(project)

  result <- sv_with_lock(project, function() {
    sv_with_db(project, function(con) {
      snapshot_id <- sv_resolve_ref_con(con, snapshot)
      if (is.null(snapshot_id)) sv_abort("Create a snapshot before recording a run.")
      run_id <- sv_id("run")
      branch <- sv_active_branch_con(con)
      environment <- sv_session_metadata(include_seed = TRUE)

      DBI::dbBegin(con)
      committed <- FALSE
      on.exit(if (!committed) DBI::dbRollback(con), add = TRUE)
      DBI::dbExecute(
        con,
        "INSERT INTO runs(id, snapshot_id, branch, started_at, command, parameters_json, environment_json, note)
         VALUES (?, ?, ?, ?, ?, ?, ?, ?)",
        params = list(
          run_id, snapshot_id, branch, sv_now(),
          sv_null(command, "Interactive research run"),
          sv_json(parameters), sv_json(environment), note
        )
      )

      artifacts <- list()
      if (length(inputs)) {
        for (path in inputs) {
          artifacts[[length(artifacts) + 1L]] <- sv_register_artifact_con(
            con, path, role = "input", snapshot_id, run_id,
            copy = copy_artifacts, metadata = list(), project = project
          )
        }
      }
      if (length(outputs)) {
        for (path in outputs) {
          artifacts[[length(artifacts) + 1L]] <- sv_register_artifact_con(
            con, path, role = "output", snapshot_id, run_id,
            copy = copy_artifacts, metadata = list(), project = project
          )
        }
      }
      sv_audit_con(
        con,
        "run_record",
        branch,
        snapshot_id,
        details = list(run_id = run_id, command = command, artifacts = artifacts)
      )
      DBI::dbCommit(con)
      committed <- TRUE
      list(id = run_id, snapshot_id = snapshot_id, branch = branch, artifacts = artifacts)
    })
  })
  sv_inform(paste0("Reproducibility record `", result$id, "` created with ", length(result$artifacts), " artifact(s)."))
  result
}

#' Write a local reproducibility report
#'
#' @param run Optional run id. When omitted, a snapshot-level report is written.
#' @param snapshot Snapshot reference, defaulting to `"HEAD"`.
#' @param path Output Markdown path. Defaults to `.scriptvault/reports/`.
#' @param project Project root. Defaults to the nearest initialized vault.
#' @return The path to the generated Markdown report.
#' @export
sv_reproducibility_report <- function(run = NULL, snapshot = "HEAD", path = NULL, project = NULL) {
  project <- sv_project_root(project)
  sv_assert_initialized(project)

  report <- sv_with_db(project, function(con) {
    snapshot_id <- sv_resolve_ref_con(con, snapshot)
    if (is.null(snapshot_id)) sv_abort("No snapshot is available for a reproducibility report.")
    snapshot_row <- DBI::dbGetQuery(con, "SELECT * FROM snapshots WHERE id = ?", params = list(snapshot_id))
    run_row <- NULL
    if (!is.null(run)) {
      run_row <- DBI::dbGetQuery(con, "SELECT * FROM runs WHERE id = ?", params = list(run))
      if (!nrow(run_row)) sv_abort(paste0("Unknown run id: `", run, "`."))
    }
    artifacts <- if (is.null(run)) {
      DBI::dbGetQuery(con, "SELECT relpath, role, hash, size_bytes, copied_object_hash, metadata_json, created_at FROM artifacts WHERE snapshot_id = ? ORDER BY created_at", params = list(snapshot_id))
    } else {
      DBI::dbGetQuery(con, "SELECT relpath, role, hash, size_bytes, copied_object_hash, metadata_json, created_at FROM artifacts WHERE run_id = ? ORDER BY created_at", params = list(run))
    }
    list(snapshot = snapshot_row[1L, , drop = FALSE], run = run_row, artifacts = artifacts)
  })

  if (is.null(path)) {
    suffix <- if (is.null(run)) report$snapshot$id[[1L]] else as.character(run)
    path <- fs::path(sv_reports_dir(project), paste0("reproducibility-", suffix, ".md"))
  } else if (!sv_is_absolute(path)) {
    path <- fs::path(project, path)
  }

  lines <- c(
    "# ScriptVault reproducibility record",
    "",
    "## Snapshot",
    "",
    paste0("- **Snapshot:** `", report$snapshot$id[[1L]], "`"),
    paste0("- **Branch:** `", report$snapshot$branch[[1L]], "`"),
    paste0("- **Created (UTC):** ", report$snapshot$created_at[[1L]]),
    paste0("- **Kind:** ", report$snapshot$kind[[1L]]),
    paste0("- **Message:** ", report$snapshot$message[[1L]]),
    paste0("- **Manifest SHA-256:** `", report$snapshot$manifest_hash[[1L]], "`"),
    paste0("- **Files in manifest:** ", report$snapshot$file_count[[1L]]),
    ""
  )

  if (!is.null(report$run) && nrow(report$run)) {
    parameters <- sv_from_json(report$run$parameters_json[[1L]], list())
    lines <- c(
      lines,
      "## Analysis run",
      "",
      paste0("- **Run ID:** `", report$run$id[[1L]], "`"),
      paste0("- **Recorded (UTC):** ", report$run$started_at[[1L]]),
      paste0("- **Command:** `", report$run$command[[1L]], "`"),
      paste0("- **Note:** ", report$run$note[[1L]]),
      "",
      "### Parameters",
      "",
      "```json",
      as.character(sv_json(parameters, pretty = TRUE)),
      "```",
      ""
    )
  }

  lines <- c(lines, "## Artifact fingerprints", "")
  if (!nrow(report$artifacts)) {
    lines <- c(lines, "No artifacts were registered for this selection.", "")
  } else {
    lines <- c(lines, "| Role | Path | SHA-256 | Bytes | Archived copy |", "|---|---|---|---:|---|")
    for (index in seq_len(nrow(report$artifacts))) {
      row <- report$artifacts[index, , drop = FALSE]
      lines <- c(lines, paste0(
        "| ", row$role[[1L]], " | `", row$relpath[[1L]], "` | `", row$hash[[1L]], "` | ",
        format(row$size_bytes[[1L]], scientific = FALSE, trim = TRUE), " | ",
        ifelse(is.na(row$copied_object_hash[[1L]]) || !nzchar(row$copied_object_hash[[1L]]), "no", "yes"), " |"
      ))
    }
    lines <- c(lines, "")
  }

  session <- sv_from_json(report$snapshot$session_json[[1L]], list())
  lines <- c(lines, "## Captured R session", "")
  if (!length(session) || is.null(session$session_info)) {
    lines <- c(lines, "No session information was stored for this snapshot.", "")
  } else {
    lines <- c(lines, "```text", unlist(session$session_info, use.names = FALSE), "```", "")
  }

  sv_atomic_write_lines(lines, path)
  sv_inform(paste0("Reproducibility report written to ", path))
  invisible(path)
}

#' Export a machine-readable local manifest
#'
#' @param ref Snapshot reference, defaulting to `"HEAD"`.
#' @param path Destination JSON path.
#' @param project Project root. Defaults to the nearest initialized vault.
#' @return The JSON file path.
#' @export
sv_export_manifest <- function(ref = "HEAD", path = NULL, project = NULL) {
  project <- sv_project_root(project)
  sv_assert_initialized(project)
  payload <- sv_with_db(project, function(con) {
    snapshot_id <- sv_resolve_ref_con(con, ref)
    if (is.null(snapshot_id)) sv_abort("Cannot export a manifest from an empty branch.")
    snapshot <- DBI::dbGetQuery(con, "SELECT * FROM snapshots WHERE id = ?", params = list(snapshot_id))
    files <- sv_manifest_con(con, snapshot_id)
    list(snapshot = snapshot, files = files, exported_at = sv_now(), project = project)
  })
  if (is.null(path)) path <- fs::path(sv_reports_dir(project), paste0("manifest-", payload$snapshot$id[[1L]], ".json"))
  if (!sv_is_absolute(path)) path <- fs::path(project, path)
  sv_atomic_write_lines(as.character(sv_json(payload, pretty = TRUE)), path)
  sv_inform(paste0("Local manifest exported to ", path))
  invisible(path)
}

#' Verify the integrity of locally archived objects
#'
#' @param full Recalculate SHA-256 hashes for every referenced archive object.
#' @param project Project root. Defaults to the nearest initialized vault.
#' @return A data frame of missing or damaged archive objects.
#' @export
sv_verify_integrity <- function(full = TRUE, project = NULL) {
  project <- sv_project_root(project)
  sv_assert_initialized(project)
  hashes <- sv_with_db(project, function(con) {
    files <- DBI::dbGetQuery(con, "SELECT DISTINCT object_hash AS hash FROM files WHERE object_hash IS NOT NULL")$hash
    artifacts <- DBI::dbGetQuery(con, "SELECT DISTINCT copied_object_hash AS hash FROM artifacts WHERE copied_object_hash IS NOT NULL")$hash
    unique(c(as.character(files), as.character(artifacts)))
  })
  hashes <- hashes[!is.na(hashes) & nzchar(hashes)]
  issues <- list()
  for (hash in hashes) {
    object <- sv_object_path(hash, project)
    if (!fs::file_exists(object)) {
      issues[[length(issues) + 1L]] <- data.frame(hash = hash, status = "missing", stringsAsFactors = FALSE)
    } else if (isTRUE(full) && !identical(sv_hash_file(object), hash)) {
      issues[[length(issues) + 1L]] <- data.frame(hash = hash, status = "hash_mismatch", stringsAsFactors = FALSE)
    }
  }
  out <- if (length(issues)) do.call(rbind, issues) else data.frame(hash = character(), status = character(), stringsAsFactors = FALSE)
  if (!nrow(out)) sv_inform("Integrity check passed: all referenced local objects are present and valid.") else sv_warn(paste0(nrow(out), " integrity issue(s) detected."))
  out
}

#' Review or remove unreferenced local archive objects
#'
#' @param dry_run Report candidate objects without deleting them.
#' @param project Project root. Defaults to the nearest initialized vault.
#' @return A data frame of orphan objects.
#' @export
sv_gc <- function(dry_run = TRUE, project = NULL) {
  project <- sv_project_root(project)
  sv_assert_initialized(project)
  referenced <- sv_with_db(project, function(con) {
    files <- DBI::dbGetQuery(con, "SELECT DISTINCT object_hash AS hash FROM files WHERE object_hash IS NOT NULL")$hash
    artifacts <- DBI::dbGetQuery(con, "SELECT DISTINCT copied_object_hash AS hash FROM artifacts WHERE copied_object_hash IS NOT NULL")$hash
    unique(c(as.character(files), as.character(artifacts)))
  })
  referenced <- referenced[!is.na(referenced) & nzchar(referenced)]
  objects <- fs::dir_ls(sv_objects_dir(project), type = "file", glob = "*.blob", fail = FALSE)
  hashes <- sub("\\.blob$", "", basename(objects))
  orphan <- objects[!(hashes %in% referenced)]
  out <- data.frame(path = as.character(orphan), hash = sub("\\.blob$", "", basename(orphan)), stringsAsFactors = FALSE)

  if (nrow(out) && !isTRUE(dry_run)) {
    unlink(out$path, force = TRUE)
    sv_with_lock(project, function() sv_with_db(project, function(con) {
      sv_audit_con(con, "gc", sv_active_branch_con(con), details = list(removed = out$hash))
    }))
    sv_inform(paste0(nrow(out), " unreferenced object(s) removed."))
  } else if (nrow(out)) {
    sv_inform(paste0(nrow(out), " unreferenced object(s) found. Re-run with `dry_run = FALSE` to remove them."))
  } else {
    sv_inform("No unreferenced local objects found.")
  }
  out
}
