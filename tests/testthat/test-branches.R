test_that("branches preserve independent local heads", {
  project <- tempfile("scriptvault-branches-")
  dir.create(project, recursive = TRUE)
  writeLines("a <- 1", file.path(project, "analysis.R"))

  sv_init(project, snapshot_initial = FALSE, quiet = TRUE)
  baseline <- sv_snapshot(project = project, quiet = TRUE)
  branch <- sv_branch_create("alternative", from = baseline$id, checkout = TRUE, project = project)
  expect_equal(branch$name[[1L]], "alternative")

  writeLines("a <- 2", file.path(project, "analysis.R"))
  alternative <- sv_snapshot(project = project, quiet = TRUE)
  sv_branch_switch("main", project = project)
  main_branches <- sv_branch_list(project)
  main_head <- main_branches$head_snapshot_id[main_branches$name == "main"]
  expect_equal(main_head[[1L]], baseline$id)
  expect_false(identical(main_head[[1L]], alternative$id))
})
