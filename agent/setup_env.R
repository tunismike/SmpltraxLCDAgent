# Install required R packages into the local libs/ directory (or user library).
script_dir <- normalizePath({
  ca <- commandArgs()
  f  <- if (any(grepl("^--file=", ca))) sub("^--file=", "", ca[grep("^--file=", ca)][1]) else ""
  if (nzchar(f)) dirname(f) else getwd()
}, winslash = "/")
root_dir <- normalizePath(file.path(script_dir, ".."), winslash = "/")
lib_dir  <- file.path(root_dir, "libs")

dir.create(lib_dir, recursive = TRUE, showWarnings = FALSE)
.libPaths(c(lib_dir, .libPaths()))

pkgs <- c("reticulate", "chromConverter", "yaml", "digest", "shiny", "shinyFiles")

need <- setdiff(pkgs, rownames(installed.packages()))
if (length(need)) {
  install.packages(need, repos = c(CRAN = "https://cloud.r-project.org"))
} else {
  message("All packages already installed.")
}

message("libPaths():")
print(.libPaths())
message("Done.")
