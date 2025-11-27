# ---- Smpltrax LCD Agent (Shimadzu .LCD -> per-sample CSV with metadata) ----

# Locate the script and repo root (works on Windows/macOS/Linux)
script_dir <- normalizePath({
  ca <- commandArgs()
  f  <- if (any(grepl("^--file=", ca))) sub("^--file=", "", ca[grep("^--file=", ca)][1]) else ""
  if (nzchar(f)) dirname(f) else getwd()
}, winslash = "/")
root_dir <- normalizePath(file.path(script_dir, ".."), winslash = "/")

suppressPackageStartupMessages({
  # Prefer local libs/ if present so this can run offline
  .libPaths(c(file.path(root_dir, "libs"), .libPaths()))
  library(yaml)
  library(digest)
})

# --- Reticulate: bind chromConverter to the Miniconda env you set up ---
# Use repo-relative defaults; fall back to system reticulate config if missing.
miniconda_path <- normalizePath(file.path(root_dir, "r-miniconda"), winslash = "/", mustWork = FALSE)
py_bin <- if (.Platform$OS.type == "windows") {
  file.path(miniconda_path, "envs", "r-reticulate", "python.exe")
} else {
  file.path(miniconda_path, "envs", "r-reticulate", "bin", "python")
}

if (dir.exists(miniconda_path)) {
  Sys.setenv(
    RETICULATE_MINICONDA_PATH = miniconda_path,
    CONDARC                   = file.path(miniconda_path, ".condarc"),
    CONDA_OVERRIDE_CHANNELS   = "1",
    RETICULATE_PYTHON_FALLBACK = "0"
  )
  if (file.exists(py_bin)) Sys.setenv(RETICULATE_PYTHON = py_bin)
}
Sys.unsetenv("VIRTUAL_ENV")

suppressPackageStartupMessages({
  library(reticulate)
  # If a specific Python was provided, prefer it; otherwise let reticulate choose.
  if (nzchar(Sys.getenv("RETICULATE_PYTHON"))) {
    try(use_python(Sys.getenv("RETICULATE_PYTHON"), required = FALSE), silent = TRUE)
  }
  if (py_available(initialize = FALSE) && !py_module_available("olefile")) {
    try(py_install("olefile", pip = TRUE), silent = TRUE)
  }
  library(chromConverter)
})

`%||%` <- function(a,b) if (is.null(a)) b else a

# -------------------------- Helpers (new + existing) --------------------------
# Vector-safe "has any non-empty in this column?"
has_nonempty_col <- function(df, col) {
  if (!is.data.frame(df) || !col %in% names(df)) return(FALSE)
  any(nzchar(as.character(df[[col]])), na.rm = TRUE)
}

# Return the first non-empty scalar from a vector/list/df column
first_nonempty <- function(x) {
  if (is.null(x)) return(NA_character_)
  if (is.data.frame(x)) x <- x[[1]]
  if (is.list(x))       x <- unlist(x, use.names = FALSE)
  x <- as.character(x)
  x <- x[nzchar(x)]
  if (length(x)) x[[1]] else NA_character_
}

# ---------------------- Resolve paths, load config ----------------------------
cfg <- yaml::read_yaml(file.path(script_dir, "config.yaml"))

resolve_path <- function(p) {
  if (!length(p) || is.null(p) || is.na(p)) return(NA_character_)
  # Absolute if starts with drive, /, or ~
  is_abs <- grepl("^[A-Za-z]:|^/|^~", p)
  normalizePath(ifelse(is_abs, p, file.path(root_dir, p)), winslash = "/", mustWork = FALSE)
}

LAB_ID      <- cfg$lab_id %||% ""
PANEL_TYPE  <- toupper(cfg$panel_type %||% "AUTO")
INPUT_DIRS  <- resolve_path(as.character(cfg$input_dirs %||% character(0)))
RECURSIVE   <- isTRUE(cfg$recursive)
PATTERN     <- cfg$file_glob %||% "*.lcd"
OUTPUT_ROOT <- resolve_path(cfg$output_root %||% file.path(root_dir, "data", "Smpltrax", "Exports"))
AGE_MIN     <- as.numeric(cfg$min_file_age_minutes %||% 2)
STABLE_WAIT <- as.numeric(cfg$stable_wait_seconds %||% 5)

dir.create(OUTPUT_ROOT, showWarnings = FALSE, recursive = TRUE)

LOG_FILE   <- file.path(OUTPUT_ROOT, "auto_convert.log")
STATE_FILE <- file.path(OUTPUT_ROOT, "processed_state.csv")

log_msg <- function(...) {
  line <- paste0(format(Sys.time(), "[%Y-%m-%d %H:%M:%S] "), sprintf(...), "\n")
  cat(line)
  cat(line, file = LOG_FILE, append = TRUE)
}

# Resume state so we don’t re-append/rewrite the same LCD repeatedly
state <- if (file.exists(STATE_FILE)) {
  tryCatch(read.csv(STATE_FILE, stringsAsFactors = FALSE),
           error = function(e) data.frame(path=character(), key=character(), ok=logical()))
} else data.frame(path=character(), key=character(), ok=logical())
seen <- unique(state$key)

file_key <- function(p) {
  info <- suppressWarnings(file.info(p))
  digest(paste0(normalizePath(p, winslash = "/"), "|", info$size, "|", as.numeric(info$mtime)), "sha1")
}
is_stable <- function(p, wait = STABLE_WAIT) {
  s1 <- suppressWarnings(file.info(p)$size)
  Sys.sleep(wait)
  s2 <- suppressWarnings(file.info(p)$size)
  isTRUE(s1 == s2 && !is.na(s1))
}

safe <- function(x) {
  x <- if (is.null(x) || is.na(x)) "" else as.character(x)
  x <- gsub("[^A-Za-z0-9._-]+", "_", x)  # keep alnum . _ -
  x <- gsub("_+", "_", x)
  x <- sub("^_+", "", x); x <- sub("_+$", "", x)
  x
}

detect_panel <- function(sample_name, fallback = "OTHER") {
  s <- toupper(sample_name %||% "")
  if (grepl("\\bCAN\\b|CANN?A?B|THC|CBD|HEMP", s)) return("CAN")
  if (grepl("\\bTRP\\b|TRPS?|TRYPT", s)) return("TRP")
  if (grepl("\\bPSTLC\\b|LCMS|LC-MS|LCMSMS|PST", s)) return("PSTLC")
  if (grepl("\\bPSTGC\\b|GCMS|GC-MS|GCFID", s)) return("PSTGC")
  if (grepl("\\bHVM\\b|HEAVY METALS", s)) return("HVM")
  if (grepl("\\bRS\\b", s)) return("RS")
  fallback
}

# ---------------- Converter for one .LCD (patched for vector safety) ----------
process_lcd <- function(lcd_path) {
  # Output folder mirrors input tree safely (or just use OUTPUT_ROOT)
  subdir <- normalizePath(dirname(lcd_path), winslash = "/")
  rel    <- gsub("[/:\\\\]+", "_", subdir)
  out_dir <- file.path(OUTPUT_ROOT, rel)
  dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

  # Try readers (long format + metadata)
  obj <- try(chromConverter::read_shimadzu_lcd(lcd_path, data_format = "long", read_metadata = TRUE), silent = TRUE)
  if (inherits(obj, "try-error") || is.null(obj)) obj <- try(chromConverter::read_sz_lcd_3d(lcd_path, data_format = "long", read_metadata = TRUE), silent = TRUE)
  if (inherits(obj, "try-error") || is.null(obj)) obj <- try(chromConverter::read_chroms(lcd_path, format = "shimadzu", read_metadata = TRUE), silent = TRUE)
  if (inherits(obj, "try-error") || is.null(obj)) stop("No reader could parse the LCD file")
  if (!is.data.frame(obj)) stop("Reader did not return a data.frame")

  # Normalize time column
  df <- obj
  names(df) <- sub("^rt$", "time_min", names(df))
  if (!"time_min" %in% names(df)) stop("No 'rt'/'time_min' column found")
  df$time_sec <- df$time_min * 60

  # Pull attributes you confirmed earlier (keep types)
  atts_keep <- c(
    "instrument","detector","detector_id","software_version","method","batch",
    "operator","run_datetime","sample_name","sample_id","vial","sample_type",
    "sample_dilution","sample_injection_volume","sample_amount",
    "time_range","time_interval","time_interval_unit","time_unit","time_multiplier",
    "wavelength","detector_y_unit","intensity_multiplier","scaled",
    "source_file","source_file_format","source_sha1","data_format","parser","format_out"
  )
  atts <- attributes(obj)[intersect(names(attributes(obj)), atts_keep)]

  meta <- lapply(atts, function(x) {
    if (inherits(x, "POSIXt")) return(x)
    if (is.logical(x) || is.numeric(x) || is.character(x)) {
      if (length(x) <= 4) return(paste(x, collapse = " | "))
      return(paste0(x[1], " … (n=", length(x), ")"))
    }
    as.character(x)[1]
  })
  meta <- as.data.frame(meta, stringsAsFactors = FALSE)

  # Derivatives for convenience
  meta$run_date  <- tryCatch({
    rd <- atts$run_datetime
    if (inherits(rd, "POSIXt")) format(rd, "%Y-%m-%d") else ""
  }, error = function(e) "")
  meta$run_time  <- tryCatch({
    rd <- atts$run_datetime
    if (inherits(rd, "POSIXt")) format(rd, "%H:%M:%S") else ""
  }, error = function(e) "")
  meta$lambda_nm <- suppressWarnings(as.numeric(gsub("[^0-9.]", "", meta$wavelength)))
  meta$y_unit    <- meta$detector_y_unit

  # -------- Vector-safe friendly channel label (no && on vectors) -------------
  n <- nrow(df)
  # base pieces as vectors of length n
  det_vec <- if ("detector" %in% names(df)) as.character(df$detector) else rep("", n)
  ch_vec  <- if ("channel"  %in% names(df)) as.character(df$channel)  else rep("", n)
  lm_vec  <- if ("lambda"   %in% names(df)) as.character(df$lambda)   else rep("", n)

  ch_has <- if ("channel" %in% names(df)) nzchar(ch_vec) else rep(FALSE, n)
  lm_has <- if ("lambda"  %in% names(df)) nzchar(lm_vec) else rep(FALSE, n)

  df$channel_label <- trimws(paste0(
    det_vec,
    ifelse(ch_has, paste0(" ", ch_vec), ""),
    ifelse(lm_has, paste0(" ", lm_vec), "")
  ))
  # ---------------------------------------------------------------------------

  # Join metadata across rows
  final <- cbind(df, meta[rep(1, nrow(df)), , drop = FALSE])

  # Reorder (time/intensity first, then key meta — harmless if some missing)
  front <- c("time_min","time_sec","intensity","unit","channel_label",
             "sample_id","sample_name","sample_type","operator","method","batch",
             "run_datetime","run_date","run_time","instrument","detector","wavelength","lambda_nm")
  front <- intersect(front, names(final))
  final <- final[, c(front, setdiff(names(final), front))]

  # ---------- Minimal naming ----------
  sid <- first_nonempty(c(final$sample_id[1],  attributes(obj)$sample_id,  tools::file_path_sans_ext(basename(lcd_path))))
  sna <- first_nonempty(c(final$sample_name[1], attributes(obj)$sample_name, ""))
  rdt <- meta$run_date[1] %||% ""

  # Resolve PANEL: GUI selection wins unless AUTO -> detect from sample_name
  panel <- if (identical(PANEL_TYPE, "AUTO")) detect_panel(sna, "OTHER") else PANEL_TYPE

  # Sanitize
  labS <- safe(LAB_ID)
  panS <- safe(panel)
  datS <- safe(rdt)
  sidS <- safe(sid)

  if (!nzchar(sidS)) stop("No sample identifier available for filename (sample_id/sample_name/basename all empty).")
  if (!nzchar(labS))  warning("LAB_ID is empty; filename will start with '__'. Consider setting lab_id in config.")

  fname  <- sprintf("%s__%s__%s__%s.csv", labS, panS, datS, sidS)
  target <- file.path(out_dir, fname)

  # Avoid accidental overwrite (suffix -2, -3, ...)
  if (file.exists(target)) {
    i <- 2
    repeat {
      target2 <- file.path(out_dir, sprintf("%s__%s__%s__%s-%d.csv", labS, panS, datS, sidS, i))
      if (!file.exists(target2)) { target <- target2; break }
      i <- i + 1
    }
  }

  # Atomic write
  tmp <- paste0(target, ".tmp")
  write.csv(final, tmp, row.names = FALSE)
  file.rename(tmp, target)

  target
  # ---------- end naming ----------
}

# ------------------------------ Discover candidates ---------------------------
lcds <- unlist(lapply(INPUT_DIRS, function(d) {
  if (!dir.exists(d)) return(character(0))
  list.files(d, pattern = glob2rx(PATTERN), full.names = TRUE, recursive = RECURSIVE)
}), use.names = FALSE)

log_msg("DEBUG: Candidate .lcd files found: %d", length(lcds))
if (!length(lcds)) {
  log_msg("No LCD files found under input_dirs.")
  quit(save = "no")
}

# -------------------------------- Process new files ---------------------------
processed <- 0L
for (p in lcds) {
  k <- file_key(p)
  if (k %in% seen) next

  info <- suppressWarnings(file.info(p))
  age  <- as.numeric(difftime(Sys.time(), info$mtime, units = "mins"))
  if (!is.finite(age) || age < AGE_MIN) next
  if (!is_stable(p)) next

  log_msg("Processing: %s", p)

  ok <- TRUE
  path_out <- NA_character_
  res <- try({
    path_out <- process_lcd(p)
  }, silent = TRUE)

  if (inherits(res, "try-error")) {
    ok <- FALSE
    log_msg("ERROR: %s", as.character(res)[1])
  } else {
    log_msg("Wrote: %s", path_out)
  }

  state <- rbind(state, data.frame(path = p, key = k, ok = ok, stringsAsFactors = FALSE))
  processed <- processed + 1L
}

write.csv(state, STATE_FILE, row.names = FALSE)
log_msg("Done. Newly processed: %d", processed)
