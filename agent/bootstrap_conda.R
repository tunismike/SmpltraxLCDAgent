# ----- Bootstrap Python for chromConverter (cross-platform) -----

script_dir <- normalizePath({
  ca <- commandArgs()
  f  <- if (any(grepl("^--file=", ca))) sub("^--file=", "", ca[grep("^--file=", ca)][1]) else ""
  if (nzchar(f)) dirname(f) else getwd()
}, winslash = "/")
root_dir <- normalizePath(file.path(script_dir, ".."), winslash = "/")
lib_dir  <- file.path(root_dir, "libs")
mini     <- file.path(root_dir, "r-miniconda")

.libPaths(c(lib_dir, .libPaths()))
Sys.setenv(RETICULATE_MINICONDA_PATH = mini)

suppressPackageStartupMessages(library(reticulate))

# 1) Ensure Miniconda exists at the repo-relative path
if (!dir.exists(mini)) {
  message(">> Installing Miniconda to: ", mini)
  install_miniconda(path = mini, force = TRUE)
}

# 2) Force conda-forge only by writing a local .condarc
condarc <- file.path(mini, ".condarc")
condarc_txt <- paste(
  "channels:",
  "  - conda-forge",
  "default_channels:",
  "  - https://conda.anaconda.org/conda-forge",
  "channel_priority: strict",
  sep = "\n"
)
writeLines(condarc_txt, condarc)
Sys.setenv(CONDARC = condarc, CONDA_OVERRIDE_CHANNELS = "1")

# 3) Create env if missing (from conda-forge only)
message(">> Checking/creating env: r-reticulate")
envs <- tryCatch(conda_list(), error = function(e) data.frame(name = character()))
if (!("r-reticulate" %in% envs$name)) {
  conda_create("r-reticulate", packages = "python", channel = "conda-forge")
}

# 4) Activate env for this R session
use_miniconda("r-reticulate", required = TRUE)

# 5) Ensure Python deps
if (!py_module_available("olefile")) py_install("olefile", pip = TRUE)

message(">> Python config:")
print(py_config())
message(">> Bootstrap complete.")
