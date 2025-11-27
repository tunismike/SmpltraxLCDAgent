# ---- Smpltrax Configurator (Shiny) ----
options(
  shiny.host = "127.0.0.1",
  shiny.port = 31813,
  shiny.launch.browser = TRUE
)

# --- Resolve script dir robustly (works for Rscript + double-click) ---
.get_script_dir <- function() {
  ca <- commandArgs()
  f  <- if (any(grepl("^--file=", ca))) sub("^--file=", "", ca[grep("^--file=", ca)][1]) else ""
  if (nzchar(f)) dirname(f) else getwd()
}
script_dir <- normalizePath(.get_script_dir(), winslash = "/")

# --- Prepend portable libs (next to your bundle) ---
portable_libs <- normalizePath(file.path(script_dir, "..", "libs"), winslash = "/", mustWork = FALSE)
if (dir.exists(portable_libs)) .libPaths(c(portable_libs, .libPaths()))

suppressPackageStartupMessages({
  library(shiny)
  library(shinyFiles)   # back to shinyFiles
  library(yaml)
  library(digest)
})

`%||%` <- function(a,b) if (is.null(a)) b else a

agent_dir <- normalizePath(script_dir, winslash = "/")
cfg_path  <- file.path(agent_dir, "config.yaml")

# --- Defaults + load config (so cfg$... exists before UI) ---
default_cfg <- list(
  lab_id = "",
  panel_type = "AUTO",             # AUTO, CAN, TRP, PSTLC, PSTGC, HVM, RS, OTHER
  input_dirs = c("C:/LabSolutions/Data"),
  recursive = TRUE,
  file_glob = "*.lcd",
  output_root = "C:/Smpltrax/Exports",
  min_file_age_minutes = 2,
  stable_wait_seconds = 5
)
cfg <- tryCatch(yaml::read_yaml(cfg_path), error = function(e) default_cfg)
for (nm in names(default_cfg)) if (is.null(cfg[[nm]])) cfg[[nm]] <- default_cfg[[nm]]

# --- Static volumes for shinyFiles ---
user_home    <- normalizePath(Sys.getenv("USERPROFILE"), winslash = "/", mustWork = FALSE)
desktop_path <- file.path(user_home, "Desktop")
extra_roots  <- if (dir.exists(desktop_path)) c(Desktop = desktop_path) else c()
vols_vec     <- c(shinyFiles::getVolumes()(), extra_roots)  # e.g. c("C:"="C:/", ...)

# ----------------------- UI -----------------------
ui <- fluidPage(
  tags$head(tags$style(HTML("
    .pill {padding:4px 8px; margin:4px 8px 0 0; background:#f1f5f9;
           border-radius:9999px; display:inline-flex; align-items:center; gap:.5rem;}
    .pill .txt {font-size:12px; color:#334155;}
    .pill .rmx {padding:0 .4rem; border-radius:9999px; line-height:1;
                background:#e2e8f0; border:1px solid #cbd5e1; cursor:pointer;}
    .pill .rmx:hover {background:#fee2e2; border-color:#fecaca;}
    .small {font-size:12px; color:#475569}
    .mt-2 { margin-top: 0.5rem; }

    /* ---- Make shinyFiles dialog folder-only (cover multiple versions) ---- */
    .shinyFiles .col-sm-7 { display: none !important; }      /* hide right pane */
    .shinyFiles .col-sm-5 { width: 100% !important; }
    .shinyFiles .shinyFiles-filetable,
    .shinyFiles .shinyFiles-files,
    .shinyFiles .table-responsive,
    .shinyFiles .well .table,
    .shinyFiles table.table { display: none !important; }
    .shinyFiles .shinyFiles-filetree, .shinyFiles .tree {
      width: 100% !important; max-width: 100% !important;
      max-height: 60vh; overflow-y: auto;
    }
    .modal-dialog .modal-body table { display: none !important; } /* last-resort */
  "))),
  titlePanel("Smpltrax LCD Agent — Configurator"),
  fluidRow(
    column(6,
      # Lab ID + Panel type
      textInput("lab_id","Lab ID (required)", value = cfg$lab_id %||% ""),
      selectInput("panel_type","Panel type",
        choices = c("AUTO","CAN","TRP","PSTLC","PSTGC","HVM","RS","OTHER"),
        selected = toupper(cfg$panel_type %||% "AUTO")),

      checkboxInput("recursive","Scan subfolders", value = isTRUE(cfg$recursive)),
      textInput("file_glob","File pattern", value = cfg$file_glob %||% "*.lcd"),
      numericInput("min_age","Min file age (minutes)", value = as.numeric(cfg$min_file_age_minutes %||% 2), min=0),
      numericInput("stable_wait","Stable wait (seconds)", value = as.numeric(cfg$stable_wait_seconds %||% 5), min=0)
    ),
    column(6,
      h4("Input folders"),
      div(class="small","Add one or more folders where .LCD files appear."),
      shinyDirButton("add_input","Add input folder…","Select folder", multiple = FALSE),
      div(class="mt-2", uiOutput("inputs_list")),

      # Selection + remove controls (bulk edits)
      selectInput("inputs_sel", "Selected inputs", choices = as.character(cfg$input_dirs), multiple = TRUE, width = "100%"),
      fluidRow(
        column(6, actionButton("remove_sel", "Remove selected")),
        column(6, actionButton("clear_inputs", "Clear all", class = "btn-danger"))
      ),

      hr(),
      h4("Output folder"),
      shinyDirButton("pick_out","Choose output folder…","Select folder"),
      div(class="mt-2", verbatimTextOutput("out_lbl", placeholder = TRUE)),
      hr(),
      actionButton("validate","Validate"),
      actionButton("save","Save"),
      actionButton("runonce","Run Agent Once", class="btn-primary"),
      br(), br(),
      verbatimTextOutput("status", placeholder = TRUE)
    )
  )
)

# --------------------- Server ---------------------
server <- function(input, output, session) {
  r <- reactiveValues(
    inputs = as.character(cfg$input_dirs),
    output_root = as.character(cfg$output_root)
  )

  observe({
    updateSelectInput(session, "inputs_sel",
      choices = r$inputs,
      selected = intersect(input$inputs_sel %||% character(0), r$inputs)
    )
  })

  # ---- Directory pickers (shinyFiles) ----
  shinyDirChoose(input, "add_input", roots = vols_vec, session = session, filetypes = c(""))
  observeEvent(input$add_input, {
    sel <- tryCatch(shinyFiles::parseDirPath(vols_vec, input$add_input),
                    error = function(e) NA_character_)
    p <- tryCatch(normalizePath(sel, winslash = "/", mustWork = FALSE),
                  error = function(e) NA_character_)
    if (is.character(p) && length(p) == 1 && !is.na(p) && nzchar(p) && dir.exists(p)) {
      if (is.null(r$inputs)) r$inputs <- character(0)
      if (!(p %in% r$inputs)) r$inputs <- c(r$inputs, p)
    } else {
      showNotification("No folder selected (or path not accessible).", type = "warning")
    }
  }, ignoreInit = TRUE)

  shinyDirChoose(input, "pick_out", roots = vols_vec, session = session, filetypes = c(""))
  observeEvent(input$pick_out, {
    sel <- tryCatch(shinyFiles::parseDirPath(vols_vec, input$pick_out),
                    error = function(e) NA_character_)
    p <- tryCatch(normalizePath(sel, winslash = "/", mustWork = FALSE),
                  error = function(e) NA_character_)
    if (is.character(p) && length(p) == 1 && !is.na(p) && nzchar(p)) {
      r$output_root <- p
    } else {
      showNotification("No output folder selected.", type = "warning")
    }
  }, ignoreInit = TRUE)

  # ---- Pills with ❌ remove buttons ----
  make_pill <- function(path) {
    id <- paste0("rm__", digest::digest(path))
    tags$span(
      class = "pill",
      tags$span(class="txt", path),
      actionLink(inputId = id, label = "\u2716", class = "rmx", title = "Remove")
    )
  }

  output$inputs_list <- renderUI({
    ins <- r$inputs
    if (!length(ins)) return(HTML("<em>None</em>"))
    do.call(tagList, lapply(ins, make_pill))
  })

  observe({
    lapply(r$inputs, function(p) {
      id <- paste0("rm__", digest::digest(p))
      if (!isTruthy(input[[id]])) {
        observeEvent(input[[id]], {
          r$inputs <- setdiff(r$inputs, p)
        }, ignoreInit = TRUE, once = TRUE)
      }
    })
  })

  # ---- Bulk removal handlers ----
  observeEvent(input$remove_sel, {
    sel <- input$inputs_sel %||% character(0)
    if (length(sel)) r$inputs <- setdiff(r$inputs, sel)
  }, ignoreInit = TRUE)

  observeEvent(input$clear_inputs, {
    r$inputs <- character(0)
  }, ignoreInit = TRUE)

  # ---- Reactive UI bits ----
  output$out_lbl <- renderText({ r$output_root })

  # ---- Buttons ----
  observeEvent(input$validate, {
    ok_in  <- length(r$inputs) && all(dir.exists(r$inputs))
    ok_out <- nzchar(r$output_root) && dir.exists(r$output_root)
    msg <- sprintf("Inputs OK: %s\nOutput OK: %s",
                   if (ok_in) "YES" else "NO",
                   if (ok_out) "YES" else "NO")
    if (!nzchar(input$lab_id)) msg <- paste(msg, "\nLab ID is empty (fill before saving).")
    output$status <- renderText(msg)
  })

  observeEvent(input$save, {
    new_cfg <- list(
      lab_id = input$lab_id,
      panel_type = toupper(input$panel_type),
      input_dirs = as.list(r$inputs),
      recursive = isTRUE(input$recursive),
      file_glob = input$file_glob,
      output_root = r$output_root,
      min_file_age_minutes = as.numeric(input$min_age),
      stable_wait_seconds = as.numeric(input$stable_wait)
    )
    dir.create(dirname(cfg_path), recursive = TRUE, showWarnings = FALSE)
    yaml::write_yaml(new_cfg, cfg_path)
    output$status <- renderText(paste("Saved to", cfg_path))
  })

  observeEvent(input$runonce, {
    rscript <- normalizePath(file.path(agent_dir, "..", "R-4.5.1", "bin", "Rscript.exe"))
    agent   <- normalizePath(file.path(agent_dir, "lcd_agent.R"))
    cmd <- sprintf('"%s" --vanilla "%s"', rscript, agent)
    out <- try(system(cmd, intern = TRUE), silent = TRUE)
    output$status <- renderText(paste(c("Ran agent:", cmd, tail(as.character(out), 10)), collapse="\n"))
  })
}

shinyApp(ui, server)
