# install all chromConverter dependencies into the portable library
.libPaths("C:/Users/Mike Tunis/Documents/SmpltraxLCDAgent/libs")
repos <- "https://cloud.r-project.org"
ap <- available.packages(repos = repos)
deps <- unique(unlist(tools::package_dependencies("chromConverter", db = ap, recursive = TRUE)))
need <- unique(c("chromConverter", deps))
miss <- setdiff(need, rownames(installed.packages(lib.loc = .libPaths())))
if (length(miss)) {
  message("Installing missing packages: ", paste(miss, collapse = ", "))
  install.packages(miss, lib = .libPaths()[1], repos = repos)
} else {
  message("All dependencies already present.")
}
