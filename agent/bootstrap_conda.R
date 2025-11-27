# ----- Bootstrap Python for chromConverter (Windows, portable) -----
.libPaths("C:/Users/Mike Tunis/Documents/SmpltraxLCDAgent/libs")
Sys.setenv(RETICULATE_MINICONDA_PATH = "C:/SmpltraxLCDAgent/r-miniconda")

suppressPackageStartupMessages(library(reticulate))

mini <- Sys.getenv("RETICULATE_MINICONDA_PATH")
conda_bat <- file.path(mini, "condabin", "conda.bat")
condarc   <- file.path(mini, ".condarc")

# 1) Ensure Miniconda exists
if (!file.exists(conda_bat)) {
  message(">> Installing Miniconda to: ", mini)
  install_miniconda(path = mini, force = TRUE)
  conda_bat <- file.path(mini, "condabin", "conda.bat")
}

if (!file.exists(conda_bat)) stop("conda.bat not found at: ", conda_bat)

# 2) Force conda-forge only by writing a local .condarc and pointing CONDARC at it
condarc_txt <- paste(
  "channels:",
  "  - conda-forge",
  "default_channels:",
  "  - https://conda.anaconda.org/conda-forge",
  "channel_priority: strict",
  sep = "\n"
)
writeLines(condarc_txt, condarc)
Sys.setenv(CONDARC = condarc)            # make conda read THIS config
Sys.setenv(CONDA_OVERRIDE_CHANNELS = "1")# ignore defaults/other sources

message(">> Using CONDARC at: ", condarc)
# (Optional) Show where config comes from:
# system2(conda_bat, c("config","--show-sources"))

# 3) Create env if missing (from conda-forge only)
message(">> Checking/creating env: r-reticulate …")
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
