# Demonstration: run this inside a disposable R project
library(scriptvault)

sv_init()
sv_watch(interval = 1)

sv_snapshot(message = "Baseline before model experiments")
sv_branch_create("sensitivity-analysis", checkout = TRUE,
                 description = "Alternative hyperparameter and spatial-resolution tests")

# Edit and save a script in RStudio, or create one for this demonstration:
writeLines(c("set.seed(42)", "x <- rnorm(100)", "mean(x)"), "analysis_demo.R")
sv_snapshot(message = "First sensitivity prototype")

sv_history()
sv_diff("analysis_demo.R")
sv_tag("demo-v1", note = "Local demonstration milestone")

sv_record_run(
  command = "analysis_demo.R",
  parameters = list(seed = 42, n = 100),
  outputs = character()
)
sv_reproducibility_report()
sv_verify_integrity()
