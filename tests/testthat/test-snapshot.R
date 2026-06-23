test_that("a snapshot archives a script and suppresses redundant versions", {
  project <- tempfile("scriptvault-project-")
  dir.create(project, recursive = TRUE)
  dir.create(file.path(project, "R"))
  writeLines(c("x <- 1", "x"), file.path(project, "R", "analysis.R"))

  sv_init(project, snapshot_initial = FALSE, quiet = TRUE)
  first <- sv_snapshot(project = project, message = "baseline", quiet = TRUE)
  expect_true(first$created)
  expect_true(nzchar(first$id))

  second <- sv_snapshot(project = project, quiet = TRUE)
  expect_false(second$created)

  writeLines(c("x <- 2", "x"), file.path(project, "R", "analysis.R"))
  third <- sv_snapshot(project = project, message = "changed", quiet = TRUE)
  expect_true(third$created)
  expect_equal(third$changed_count, 1L)

  history <- sv_file_history("R/analysis.R", project = project)
  expect_gte(nrow(history$data), 2L)
  expect_true(file.exists(file.path(project, ".scriptvault", "vault.sqlite")))
})

test_that("a single file can be restored from a previous local snapshot", {
  project <- tempfile("scriptvault-restore-")
  dir.create(project, recursive = TRUE)
  writeLines("value <- 1", file.path(project, "analysis.R"))

  sv_init(project, snapshot_initial = FALSE, quiet = TRUE)
  baseline <- sv_snapshot(project = project, quiet = TRUE)
  writeLines("value <- 2", file.path(project, "analysis.R"))
  sv_snapshot(project = project, quiet = TRUE)

  restored <- sv_restore("analysis.R", ref = baseline$id, project = project, quiet = TRUE)
  expect_true(restored$created)
  expect_identical(readLines(file.path(project, "analysis.R")), "value <- 1")
  recovery <- file.path(project, ".scriptvault", "recovery")
  expect_true(length(list.files(recovery, recursive = TRUE, all.files = TRUE)) > 0L)
})
