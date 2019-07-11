
# UI Function -------------------------------------------------------------

dagPreviewUI <- function(id, include_graph_downloads = TRUE) {
  ns <- shiny::NS(id)
  
  class_3_col <- "col-md-4 col-md-offset-0 col-sm-8 col-sm-offset-2 col-xs-12"
  
  download_choices <- c(
    "PDF" = "pdf",
    "PNG" = "png",
    "LaTeX TikZ" = "tikz"
  )
  
  if (include_graph_downloads) {
    download_choices <- c(
      download_choices,
      "dagitty (R: RDS)" = "dag_dagitty",
      "ggdag (R: RDS)" = "dag_tidy"
    )
  }
  
  tagList(
    fluidRow(
      column(
        width = 12, 
        align = "center", 
        shinyjs::hidden(tags$div(
          id = ns("tikzOut-help"),
          class="alert alert-danger",
          role="alert",
          HTML(
            "<p>An error occurred while compiling the preview.",
            "Are there syntax errors in your labels?",
            "Single <code>$</code> need to be escaped: <code>\\$</code>.</p>"
          )
        )),
        shinycssloaders::withSpinner(uiOutput(ns("tikzOut")), color = "#C4C4C4")
      )
    ),
    fluidRow(
      tags$div(
        class = class_3_col,
        tags$div(
          id = ns("showPreviewContainer"),
          prettySwitch(ns("showPreview"), "Preview DAG", status = "primary", fill = TRUE)
          # uiOutput(ns("showPreview_helptext"))
        )
      ),
      tags$div(
        class = class_3_col,
        selectInput(
          inputId = ns("downloadType"),
          label = "Type of download",
          choices = download_choices
        ),
        uiOutput(ns("downloadType_helptext"))
      ),
      tags$div(
        class = paste(class_3_col, "dagpreview-download-ui"),
        div(
          class = "btn-group",
          role = "group",
          id = ns("download-buttons"),
          downloadButton(ns("downloadButton"))
        )
      )
    )
  )
}


# Server Module -----------------------------------------------------------

# This module takes tikz code and creates DAG preview content and returns TRUE
# or FALSE value to track whether the preview is visible.
dagPreview <- function(
  input, output, session, 
  session_dir, 
  tikz_code, 
  dag_dagitty = reactive(NULL), 
  dag_tidy = reactive(NULL)
) {
  ns <- session$ns
  SESSION_TEMPDIR <- file.path(session_dir, sub("-$", "", ns("")))
  
  tikz_cache_dir <- reactiveVal(NULL)
  
  # Render tikz preview ----
  observe({
    req(input$showPreview)
    
    tikz_lines <- tikz_code()
    
    useLib <- "\\usetikzlibrary{matrix,arrows,decorations.pathmorphing}"
    
    pkgs <- paste(buildUsepackage(pkg = list("tikz"), uselibrary = useLib), collapse = "\n")
    
    preview_dir <- tex_cached_preview(
      session_dir = SESSION_TEMPDIR,
      obj = tikz_lines,
      stem = "DAGimage",
      imgFormat = "png",
      returnType = "shiny",
      density = tex_opts$get("density"),
      keep_pdf = TRUE,
      usrPackages = pkgs,
      margin = tex_opts$get("margin"),
      cleanup = tex_opts$get("cleanup")
    )
    tikz_cache_dir(preview_dir)
  }, priority = -100)
  
  # Create tikz preview UI ----
  output$tikzOut <- renderUI({
    req(tikz_code(), input$showPreview)
    
    if (is.null(tikz_cache_dir())) return()
    if (!length(tikz_cache_dir())) {
      shinyjs::show("tikzOut-help")
      return()
    } else {
      shinyjs::hide("tikzOut-help")
    }
    
    image_path <- file.path(tikz_cache_dir(), "DAGimage.png")
    if (!file.exists(image_path)) {
      debug_line("Image does not exist: ", image_path)
      return()
    }
    
    image_tmp <- tempfile("dag_image_", SESSION_TEMPDIR, ".png")
    file.copy(image_path, image_tmp)
    debug_line("Serving image: ", image_tmp)
    
    tags$img(
      src = sub("www/", "", image_tmp, fixed = TRUE),
      contentType = "image/png",
      style = "max-width: 100%; max-height: 600px; -o-object-fit: contain;",
      alt = "DAG"
    )
  })
  
  output$downloadType_helptext <- renderUI({
    is_tikz_download <- input$downloadType %in% c("pdf", "png", "tikz")
    if (is_tikz_download && !input$showPreview) {
      shinyjs::disable("downloadButton")
      return(helpText("Please preview DAG to enable downloads"))
    }
    
    if (!is_tikz_download && !nrow(edge_frame(rve$edges, rvn$nodes))) {
      shinyjs::disable("downloadButton")
      return(helpText("Please add at least one edge to the DAG"))
    }
    
    if (!length(tikz_cache_dir())) {
      shinyjs::disable("downloadButton")
      return()
    }
    
    shinyjs::enable("downloadButton")
  })
  
  output$downloadButton <- downloadHandler(
    filename = function() {
      paste0(
        "DAG.", 
        switch(
          input$downloadType,
          "dagitty" =,
          "ggdag" = "rds",
          "tikz" = "tex",
          "png" = "png",
          "pdf" = "pdf"
        )
      )
    },
    content = function(file) {
      if (input$downloadType == "pdf") {
        
        file.copy(file.path(tikz_cache_dir(), "DAGimageDoc.pdf"), file)
        
      } else if (input$downloadType == "png") {
        
        file.copy(file.path(tikz_cache_dir(), "DAGimage.png"), file)
        
      } else if (input$downloadType == "tikz") {
        
        merge_tex_files(
          file.path(tikz_cache_dir(), "DAGimageDoc.tex"),
          file.path(tikz_cache_dir(), "DAGimage.tex"),
          file
        )
        
      } else if (input$downloadType == "dag_dagitty") {
        
        if (is.null(dag_dagitty())) return(NULL)
        
        saveRDS(dag_dagitty(), file = file)
        
      } else if (input$downloadType == "dag_tidy") {
        
        if (is.null(dag_tidy())) return(NULL)
        
        saveRDS(dag_tidy(), file = file)
      }
    },
    contentType = NA
  )
  
  return(reactive(input$showPreview))
}


# Helper Functions --------------------------------------------------------

tex_cached_preview <- function(session_dir, ...) {
  # Takes arguments for texPreview() except for fileDir
  # hashes inputs and then writes preview into session_dir/args_hash
  # Skips rendering if the cache already exists
  # Returns directory containing the preview documents
  
  args <- list(...)
  args_hash <- digest::digest(args)
  
  cache_dir <- file.path(session_dir, args_hash)
  
  if (dir.exists(cache_dir)) {
    return(cache_dir)
  }
  
  dir.create(cache_dir, recursive = TRUE)
  args$fileDir <- cache_dir
  tryCatch({
    do.call("texPreview", args)
    cache_dir
  }, error = function(e) {
    unlink(cache_dir, recursive = TRUE)
    character()
  })
}

# Merge tikz TeX source into main TeX file
merge_tex_files <- function(main_file, input_file, out_file) {
  x <- readLines(main_file)
  y <- readLines(input_file)
  which_line <- grep("input{", x, fixed = TRUE)
  which_line <- intersect(which_line, grep(basename(input_file), x))
  x[which_line] <- paste(y, collapse = "\n")
  writeLines(x, out_file)
}

