# ScriptVault local installer
# Run this script from the directory that contains scriptvault_0.1.0.tar.gz.

options(repos = c(CRAN = "https://cloud.r-project.org"))
required <- c("DBI", "RSQLite", "digest", "filelock", "fs", "jsonlite")
optional <- c("later", "rstudioapi")
missing <- required[!vapply(required, requireNamespace, logical(1L), quietly = TRUE)]
if (length(missing)) install.packages(missing)
missing_optional <- optional[!vapply(optional, requireNamespace, logical(1L), quietly = TRUE)]
if (length(missing_optional)) install.packages(missing_optional)

archive <- file.path(getwd(), "scriptvault_0.1.0.tar.gz")
if (!file.exists(archive)) {
  stop("Could not find scriptvault_0.1.0.tar.gz in the current working directory.", call. = FALSE)
}
install.packages(archive, repos = NULL, type = "source")
message("ScriptVault installed. Restart RStudio, then run: library(scriptvault)")
