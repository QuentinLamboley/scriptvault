test_that("artifact metadata and a report can be created locally", {
  project <- tempfile("scriptvault-repro-")
  dir.create(project, recursive = TRUE)
  writeLines("result <- 1", file.path(project, "analysis.R"))
  writeLines("value", file.path(project, "output.txt"))

  sv_init(project, snapshot_initial = FALSE, quiet = TRUE)
  sv_snapshot(project = project, quiet = TRUE)
  artifact <- sv_artifact_register("output.txt", role = "output", project = project)
  expect_true(nzchar(artifact$hash))
  run <- sv_record_run(command = "analysis.R", outputs = "output.txt", project = project)
  report <- sv_reproducibility_report(run = run$id, project = project)
  expect_true(file.exists(report))
})
