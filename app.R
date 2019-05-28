library(shiny)
library(shinydashboard)
library(DiagrammeR)
library(dagitty)
library(stringr)
library(igraph)
library(texPreview)
library(shinyAce)
library(shinyBS)
library(dplyr)
library(ggdag)
library(shinyWidgets)
library(shinyjs)
# Additional libraries: tidyr, digest, rlang

tex_opts$set(list(
  density = 1200,
  margin = list(left = 0, top = 0, right = 0, bottom = 0),
  cleanup = c("aux", "log")
))


# Functions ---------------------------------------------------------------

DEBUG <- getOption("shinydag.debug", FALSE)
debug_input <- function(x, x_name = NULL) {
  if (!isTRUE(DEBUG)) return()
  
  if (inherits(x, "igraph")) {
    cat("", capture.output(print(x)), sep = "\n")
  } else if (length(x) == 1 && !is.list(x)) {
    cat("\n", if (!is.null(x_name)) paste0(x_name, ":"), if (length(names(x))) names(x), "-", x)
  } else {
    if (!inherits(x, "data.frame")) x <- tibble::enframe(x)
    cat("", if (!is.null(x_name)) paste0(x_name, ":"), knitr::kable(x), sep = "\n")
  }
}
debug_line <- function(...) {
  if (!isTRUE(DEBUG)) return()
  cli::cat_line(...)
}


buildUsepackage <- if (length(find("build_usepackage"))) texPreview::build_usepackage else texPreview::buildUsepackage

`%||%` <- function(x, y) if (is.null(x)) y else x
`%??%` <- function(x, y) if (!is.null(x) && x != "") y

warnNotification <- function(...) showNotification(
  paste0(...), duration = 5, closeButton = TRUE, type = "warning"
)

invertNames <- function(x) setNames(names(x), unname(x))

# UI ----------------------------------------------------------------------

ui <- dashboardPage(
  title = "shinyDAG",
  dashboardHeader(disable = TRUE),
  dashboardSidebar(disable = TRUE),
  dashboardBody(
    shinyjs::useShinyjs(),
    fluidRow(
      # ---- Box: DAG ----
      box(
        title = "shinyDAG",
        column(12, align = "center", uiOutput("tikzOut")),
        # column(12, align = "center", imageOutput("tikzOut")),
        tags$style(
          type = "text/css",
          "#showPreviewContainer { padding-top: 32px; }",
          "#downloadButton { margin-top: 25px; }"
        ),
        fluidRow(
          column(4, tags$div(id = "showPreviewContainer", 
                            prettySwitch("showPreview", "Preview DAG", status = "primary", fill = TRUE))),
          column(4, 
            selectInput("downloadType", "Type of download",
                        choices = list("PDF" = 4, "PNG" = 3, "LaTeX TikZ" = 2, "dagitty R object" = 1, "ggdag R object" = 5)
            )
          ),
          column(4, downloadButton("downloadButton"))
        ),
        # br(), br(),
        prettySwitch(
          inputId = "showFlow",
          label = "Examine DAG elements",
          status = "primary",
          fill = TRUE
        ),
        conditionalPanel(
          condition = "input.showFlow == 1",
          # textOutput("adjustText"),
          # verbatimTextOutput("adjustSets"),
          fluidRow(
            column(6, "Open paths", verbatimTextOutput("openPaths")),
            column(6, "Closed paths", verbatimTextOutput("closedPaths"))
          )
        )
      ),
      # ---- Box: Controls ----
      tabBox(
        title = div(img(src = "GerkeLab.png", width = 40, height = 40)),
        # ---- Tab: Build ----
        tabPanel(
          "Build",
          tags$style(
            type = "text/css",
            ".shiny-output-error { visibility: hidden; }",
            ".shiny-output-error:before { visibility: hidden; }",
            "#node_delete { margin-top: 20px; color: #FFF }",
            "#edge_btn { margin-top: 25px; color: #FFF }",
            "@media (min-width: 768px) { #node_delete { margin-left: -25px; } }"
          ),
          uiOutput("nodeListButtonsLabel"),
          uiOutput("nodeListButtons"),
          fluidRow(
            column(10, 
              searchInput("nodeLabel", label = "", value = "", placeholder = NULL,
                          btnSearch = icon("check"), btnReset = icon("backspace"), width = "100%")
            ),
            column(2, uiOutput("node_ui_remove"))
          ),
          # checkboxInput("clickType", "Click to remove a node", value = FALSE),
          plotOutput("clickPad", click = "pad_click"),
          fluidRow(
            column(5, selectizeInput("from_edge", "Parent Node", choices = c("Add a node to the plot area" = ""))),
            column(5, selectizeInput("to_edge", "Child Node", choices = c("Add a node to the plot area" = ""))),
            column(2, uiOutput("ui_edge_btn"))
          ),
          checkboxGroupInput("adjustNode", "Select nodes to adjust", inline = TRUE),
          radioButtons("exposureNode", "Exposure", choices = c("None" = ""), inline = TRUE),
          radioButtons("outcomeNode", "Outcome", choices = c("None" = ""), inline = TRUE)
        ),
        # ---- Tab: Edit Aesthetics ----
        tabPanel(
          "Edit aesthetics",
          selectInput("arrowShape", "Select arrow head", choices = c(
            "stealth", "stealth'", "diamond",
            "triangle 90", "hooks", "triangle 45",
            "triangle 60", "hooks reversed", "*"
          ), selected = "stealth"),
          uiOutput("curveAngle"),
          helpText("A negative degree will change the orientation of the curve."),
          fluidRow(
            column(4, uiOutput("curveColor")),
            column(4, uiOutput("curveLty")),
            column(4, uiOutput("curveThick"))
          )
        ),
        # ---- Tab: Edit LaTeX ----
        tabPanel(
          "Edit LaTeX",
          helpText("WARNING: Editing code here will only change the appearance of the DAG and not the information on paths provided."),
          uiOutput("texEdit"),
          actionButton("redoTex", "Initiate Editing!"),
          conditionalPanel(
            condition = "input.redoTex == 1",
            uiOutput("tikzOutNew"),
            selectInput("downloadType2", "Type of download",
              choices = list("PDF" = 3, "PNG" = 2, "LaTeX TikZ" = 1)
            ),
            downloadButton("downloadButton2")
          )
        ),
        # ---- Tab: About ----
        tabPanel(
          "About shinyDAG",
          h6("Development Team: Jordan Creed, Travis Gerke, and Garrick Aden-Buie"),
          h6("For more information on our lab and other projects please check out our website at", tags$a(href = "http://gerkelab.com", "gerkelab.com")),
          h6("All code is available on GitHub at", tags$a(href = "https://github.com/GerkeLab/ShinyDAG", "GerkeLab/ShinyDag")),
          h6("Any errors or comments can be directed to", 
             tags$a(href = "mailto:travis.gerke@moffitt.org", "travis.gerke@moffitt.org"), 
             "or", tags$a(href = "mailto:jordan.h.creed@moffitt.org", "jordan.h.creed@moffitt.org")
          )
        )
      )
    )
  )
)


# Server ------------------------------------------------------------------
server <- function(input, output, session) {
  # ---- Session Temp Directory ----
  SESSION_TEMPDIR <- file.path("www", session$token)
  dir.create(SESSION_TEMPDIR, showWarnings = FALSE)
  onStop(function() {
    message("Removing session tempdir: ", SESSION_TEMPDIR)
    unlink(SESSION_TEMPDIR, recursive = TRUE)
  })
  message("Using session tempdir: ", SESSION_TEMPDIR)


  
  # ---- Reactive Values ----
  rv <- reactiveValues(
    g    = make_empty_graph(),
    gg   = make_empty_graph(),
    edges = list(),
    nodes = list(),
    pts  = list(x = vector("numeric", 0), y = vector("numeric", 0), name = vector("character", 0)),
    pts2 = tibble(x = rep(1:7, each = 7), y = rep(1:7, 7), name = rep(NA, 49))
  )
  
  node_list_btn_last_state <- c()
  
  # rv$edges is a named list, e.g. for hash(A) -> hash(B):
  # rv$edges[edge_key(hash(A), hash(B))] = list(from = hash(A), to = hash(B))
  
  # rv$nodes is a named list where name is a hash
  # rv$nodes$abcdefg = list(name, x, y)
  
  # rv$gg rebuilds whenever nodes or edges (or options?) change
  observe({
    req(length(rv$nodes))
    # debug_line("Rebuilding graph")
    g <- make_empty_graph()
    if (nrow(node_frame(rv$nodes))) {
      g <- g + node_vertices(rv$nodes)
    }
    if (length(rv$edges)) {
      # Add edges
      g <- g + edge_edges(rv$edges, rv$nodes)
    }
    
    rv$gg <- g
    debug_input(rv$gg, "rv$gg")
  })
  
  # ---- Node Helper Functions ----
  node_new <- function(nodes, hash, name) {
    nodes[[hash]] <- list(name = name, x = NA, y = NA)
    nodes
  }
  
  node_name_valid <- function(nodes, name, warn = FALSE) {
    if (!nzchar(name)) {
      warnNotification("Please specify a name for the node")
      return(FALSE)
    }
    name_in_nodes <- vapply(nodes, function(n) name == n$name, FALSE)
    if (any(name_in_nodes)) {
      if (warn) warnNotification('"', name, '" is already the name of a node')
      FALSE
    } else TRUE
  }
  
  node_names <- function(nodes, all = FALSE) {
    if (!length(nodes)) return(character())
    x <- invertNames(sapply(nodes, function(x) x$name))
    if (all) return(x)
    has_position <- sapply(nodes, function(x) !is.na(x$x))
    x[has_position]
  }
  
  node_update <- function(nodes, hash, name = NULL, x = NULL, y = NULL) {
    nodes[[hash]]$name <- name %||% nodes[[hash]]$name
    nodes[[hash]]$x    <-    x %||% nodes[[hash]]$x
    nodes[[hash]]$y    <-    y %||% nodes[[hash]]$y
    nodes
  }
  
  node_delete <- function(nodes, hash) {
    .nodes <- nodes[setdiff(names(nodes), hash)]
    if (length(.nodes)) .nodes else list()
  }
  
  node_frame <- function(nodes, full = FALSE) {
    if (!length(nodes)) return(tibble())
    x <- bind_rows(nodes) %>% 
      mutate(hash = names(nodes)) %>% 
      select(hash, everything())
    if (full) return(x)
    filter(x, !is.na(x))
  }
  
  node_vertices <- function(nodes) {
    v_df <- node_frame(nodes)
    vertices(
      name = v_df$name,
      x = v_df$x,
      y = v_df$y,
      hash = v_df$hash
    )
  }
  
  node_nearest <- function(nodes, coordinfo, threshold = 0.5) {
    nodes %>% 
      node_frame() %>% 
      mutate(dist = (x - coordinfo$x)^2 + (y - coordinfo$y)^2) %>% 
      arrange(dist) %>% 
      filter(dist <= threshold) %>% 
      slice(1) %>% 
      select(-dist)
  }

  # ---- Node Controls ----
  node_btn_id <- function(node_hash) paste0("node_toggle_", node_hash)
  node_btn_get_hash <- function(node_btn_id) sub("node_toggle_", "", node_btn_id, fixed = TRUE)
  
  # Add or modify node label on search button
  observeEvent(input$nodeLabel_search, {
    node_list_btn_now <- isolate(node_list_btn_state())
    if (!node_name_valid(rv$nodes, input$nodeLabel, warn = !any(node_list_btn_now))) {
      return(NULL)
    }
    if (!length(node_list_btn_now) || !any(node_list_btn_now)) {
      # Node is new if a node label button is not toggled
      new_node_hash <- digest::digest(Sys.time())
      if (!new_node_hash %in% names(rv$nodes)) {
        # Update node list
        rv$nodes <- node_new(rv$nodes, new_node_hash, input$nodeLabel)
        # Update node btn state
        new_node_state <- setNames(TRUE, node_btn_id(new_node_hash))
        node_list_btn_last_state <<- c(node_list_btn_last_state, new_node_state)
        node_last_change_was_app(TRUE)
        # Clear search input
        updateSearchInput(session, "nodeLabel", value = "")
      }
    } else {
      # Editing mode (node label button toggled)
      s_node_list_btn <- isolate(node_list_btn_sel()) %>% node_btn_get_hash()
      rv$nodes <- node_update(rv$nodes, s_node_list_btn, input$nodeLabel)
    }
    debug_input(rv$nodes, "rv$nodes")
  })
  
  # Untoggle node label on reset button (also clears text as well)
  observeEvent(input$nodeLabel_reset, {
    node_list_btn_now <- isolate(node_list_btn_state())
    if (length(node_list_btn_now) && any(node_list_btn_now)) {
      updateButton(session, isolate(node_list_btn_sel()), value = FALSE)
    }
  })
  
  output$nodeListButtonsLabel <- renderUI({
    if (!length(rv$nodes)) {
      tags$p(tags$strong("Add a Node"))
    } else if (is.null(node_list_btn_sel())) {
      tags$p(tags$strong("Add Node or Select Existing to Edit or Place"))
    } else {
      tags$p(tags$strong("Edit or Place Selected Node"))
    }
  })
  
  output$nodeListButtons <- renderUI({
    req(rv$nodes)
    if (!length(rv$nodes)) return()
    node_list_buttons <- vector("list", length(rv$nodes))
    for (i in seq_along(rv$nodes)) {
      node_btn_id.this <- node_btn_id(names(rv$nodes)[i])
      node_list_buttons[[i]] <- bsButton(
        node_btn_id.this,
        value = node_list_btn_last_state[node_btn_id.this],
        label = rv$nodes[[i]]$name,
        type = "toggle"
      )
    }
    tagList(
      tags$div(
        class = "btn-group",
        node_list_buttons,
        bsButton("nodeLabelAddNew", "", icon = icon("plus"), style = "primary")
      )
    )
  })
  
  # Handle new node button triggered
  observeEvent(input$nodeLabelAddNew, {
    node_list_btn_now <- node_list_btn_state()
    if (length(node_list_btn_now) && any(node_list_btn_now)) {
      updateButton(session, isolate(node_list_btn_sel()), value = FALSE)
    }
  })
  
  # Disable new node button if in "add node" state
  observe({
    node_list_btn_now <- node_list_btn_sel()
    shinyjs::toggleState("nodeLabelAddNew", condition = !is.null(node_list_btn_now))
  })
  
  # Current state of the node buttons, zero-length if no nodes yet
  node_list_btn_state <- reactive({
    node_list_input_ids <- grep("node_toggle", names(input), value = TRUE, fixed = TRUE)
    vapply(node_list_input_ids, function(n) input[[n]] %||% FALSE, FALSE)
  })
  
  # ID of currently toggled button (otherwise NULL)
  node_list_btn_sel <- reactive({
    if (is.null(node_list_btn_state())) {
      NULL
    } else if (any(node_list_btn_state()) && sum(node_list_btn_state()) == 1) {
      names(node_list_btn_state())[node_list_btn_state()]
    } else {
      NULL
    }
  })
  
  node_last_change_was_app <- reactiveVal(TRUE)
  
  # Only one node toggled at a time
  observe({
    req(length(node_list_btn_state()) > 0)
    debug_input(isolate(node_last_change_was_app()), "node_last_change_was_app()")
    debug_input(node_list_btn_last_state, "node_list_btn_last_state")
    debug_input(node_list_btn_state(), "node_list_btn_state()")
    
    if (isolate(node_last_change_was_app())) {
      # The app manually updated the button state, do nothing
      node_last_change_was_app(FALSE)
    } else {
      # Use node_list_btn_last_state to determine which button was changed by user
      # Last toggled button wins, all others turned off
      node_list_btn_state_now <- node_list_btn_state()[names(node_list_btn_last_state)]
      button_changed <- which(node_list_btn_state_now != node_list_btn_last_state)
      if (length(button_changed) && sum(node_list_btn_state_now) > 1) {
        button_turn_off <- setdiff(which(node_list_btn_state_now), button_changed)
        for (i in button_turn_off) {
          debug_line("Updating: ", names(node_list_btn_state_now)[i])
          updateButton(session, names(node_list_btn_state_now)[i], value = FALSE)
          node_last_change_was_app(TRUE)
        }
      }
    }
    node_list_btn_last_state <<- vapply(
      names(node_list_btn_last_state),
      function(n) input[[n]] %||% FALSE, 
      FALSE
    )
  }, priority = -10)
  
  # Selecting existing node label button enables editing
  observe({
    req(node_list_btn_sel())
    s_node_list_btn_id <- node_list_btn_sel() %>% node_btn_get_hash()
    s_node_name <- isolate(rv$nodes[[s_node_list_btn_id]]$name)
    updateSearchInput(session, "nodeLabel", value = unname(s_node_name), label = "")
  })
  
  # Selecting existing node label enables node delete button
  output$node_ui_remove <- renderUI({
    req(node_list_btn_sel())
    if (node_btn_sel_has_point()) {
      actionButton(
        "node_delete", "", 
        icon = icon("eraser"), 
        class = "btn-warning", 
        alt = "Remove Node from DAG",
        `data-toggle` = "tooltip", 
        `data-placement` = "bottom",  
        title = "Remove Node from DAG"
      )
    } else {
      actionButton(
        "node_delete", "", 
        icon = icon("trash"), 
        class = "btn-danger", 
        alt = "Delete Node",
        `data-toggle` = "tooltip", 
        `data-placement` = "bottom",  
        title = "Delete Node"
      )
    }
  })
  
  # Erase or delete button?
  node_btn_sel_has_point <- reactive({
    req(node_list_btn_sel())
    node_btn_hash <- node_btn_get_hash(node_list_btn_sel())
    !is.null(rv$nodes[[node_btn_hash]]) && !is.na(rv$nodes[[node_btn_hash]]$x)
  })
  
  # Delete node
  observeEvent(input$node_delete, {
    s_node_btn_hash <- node_btn_get_hash(node_list_btn_sel())
    if (node_btn_sel_has_point()) {
      rv$nodes <- node_update(rv$nodes, s_node_btn_hash, x = NA, y = NA)
    } else {
      rv$nodes <- node_delete(rv$nodes, s_node_btn_hash)
      updateButton(session, node_list_btn_sel(), value = FALSE)
    }
    shinyjs::runjs("$('#node_delete').tooltip('hide')")
    debug_input(rv$nodes, "rv$nodes")
    debug_input(node_list_btn_state(), "node_list_btn_state()")
  })
  
  # Clear nodeLabel input when no node buttons selected
  observe({
    req(!any(node_list_btn_state()))
    # Clear node input
    updateSearchInput(session, "nodeLabel", value = "", label = "")
  })

  # ---- Click Pad ----
  # Add or move point on clickPad
  observeEvent(input$pad_click, {
    if (!length(rv$nodes)) {
      warnNotification("Please add a node")
      return()
    }
    
    nearest_node <- node_nearest(rv$nodes, input$pad_click)
    if (is.null(node_list_btn_sel())) {
      if (nrow(nearest_node)) {
        # Select node when clicked on
        updateButton(session, node_btn_id(nearest_node$hash), value = TRUE)
      } else {
        # No node clicked or selected
        warnNotification("Please select a node")
      }
      return()
    } else if (nrow(nearest_node)) {
      # Switch node selection if A selected and B clicked on
      s_node_btn_hash <- node_list_btn_sel() %>% node_btn_get_hash()
      # Deselect clicked-on node if already selected
      if (isTRUE(nearest_node$hash == s_node_btn_hash)) {
        updateButton(session, node_list_btn_sel(), value = FALSE)
      } else {
        updateButton(session, node_list_btn_sel(), value = FALSE)
        updateButton(session, node_btn_id(nearest_node$hash), value = TRUE)
      }
      return()
    }
    
    node_hash <- node_btn_get_hash(node_list_btn_sel())
    
    rv$nodes <- node_update(
      rv$nodes, 
      node_hash, 
      x = round(input$pad_click$x),
      y = round(input$pad_click$y))
    
    # debug_line("Rebuilding graph")
    # rv$gg <- make_empty_graph() + 
    #   node_vertices(rv$nodes)
    
    debug_input(rv$nodes, "rv$nodes")
  })

  # clickPad display
  output$clickPad <- renderPlot({
    req(rv$nodes)
    rv_pts <- node_frame(rv$nodes)
    
    if (nrow(rv_pts)) {
      rv_pts$color <- if (!is.null(node_list_btn_sel())) {
        ifelse(rv_pts$hash == node_btn_get_hash(node_list_btn_sel()),
               "firebrick1", "black")
      } else "black"
      plot(rv_pts$x, rv_pts$y, xlim = c(1, 7), ylim = c(1, 7), bty = "n", xaxt = "n", yaxt = "n", ylab = "", xlab = "", xaxs = "i", col = "white")
      grid()
      if (length(rv$edges)) {
        e_pts <- edge_points(rv$edges, rv$nodes, push_by = 0.05)
        for (i in seq_len(nrow(e_pts))) {
          arrows(
            e_pts$from.x[i], e_pts$from.y[i],
            e_pts$to.x[i], e_pts$to.y[i],
            col = e_pts$color[i],
            lty = e_pts$lty[i], 
            length = 0.1
          )
        }
      }
      text(rv_pts$x, rv_pts$y, labels = rv_pts$name, cex = 2, col = rv_pts$color)
    } else {
      plot(NA, NA, xlim = c(1, 7), ylim = c(1, 7), bty = "n", xaxt = "n", yaxt = "n", ylab = "", xlab = "", xaxs = "i")
      grid()
    }
  })

  # ---- Node - Options ----
  update_node_options <- function(nodes, inputId, updateFn, none_choice = TRUE, ...) {
    available_choices <- c("None" = "", node_names(nodes))
    if (!none_choice) available_choices <- available_choices[-1]
    s_choice <- intersect(isolate(input[[inputId]]), available_choices)
    if (!length(s_choice) && none_choice) s_choice <- ""
    
    updateFn(session, 
             inputId, 
             choices = available_choices, 
             selected = s_choice,
             ...
    )
  }
  observe({
    update_node_options(rv$nodes, "adjustNode", none_choice = FALSE, 
                        updateCheckboxGroupInput, inline = TRUE)
    update_node_options(rv$nodes, "exposureNode", updateRadioButtons, inline = TRUE)
    update_node_options(rv$nodes, "outcomeNode", updateRadioButtons, inline = TRUE)
  })

  output$adjustText <- renderText({
    if (is.null(input$exposureNode) & is.null(input$outcomeNode)) {
      paste0("Minimal sufficient adjustment sets")
    } else {
      paste0(
        "Minimal sufficient adjustment set(s) to estimate the effect of ",
        input$exposureNode, " on ", input$outcomeNode
      )
    }
  })
  
  # ---- Edges - Add/Remove ----
  edge_key <- function(x, y) digest::digest(c(x, y))
  
  edge_frame <- function(edges, nodes, ...) {
    dots <- rlang::enexprs(...)
    bind_rows(edges) %>% 
      mutate(hash = names(edges)) %>% 
      tidyr::gather(position, node_hash, from:to) %>% 
      left_join(select(node_frame(nodes), hash, name), 
                by = c("node_hash" = "hash")) %>% 
      select(-node_hash) %>% 
      tidyr::spread(position, name) %>% 
      mutate(!!!dots)
  }
  
  edge_edges <- function(edges, nodes, ...) {
    do.call(edge, as.list(edge_frame(edges, nodes, ...)))
  }
  
  edge_points <- function(edges, nodes, push_by = 0) {
    e_df <- edge_frame(edges, nodes) 
    
    e_df %>% 
      select(hash, from, to) %>% 
      tidyr::gather(key, name, -hash) %>% 
      left_join(node_frame(nodes)[, -1], by = "name") %>% 
      tidyr::gather(var, pt, x:y) %>% 
      mutate(var = paste(key, var, sep = ".")) %>% 
      select(-name, -key) %>% 
      tidyr::spread(var, pt) %>% 
      left_join(e_df, by = "hash") %>% 
      mutate(
        d_x = to.x - from.x, 
        d_y = to.y - from.y,
        from.x = from.x + push_by * d_x, 
        from.y = from.y + push_by * d_y, 
        to.x   = to.x   - push_by * d_x, 
        to.y   = to.y   - push_by * d_y
      )
  }
  
  # Update Parent/Child node selection for edges
  observe({
    node_choices <- node_names(rv$nodes)
    if (length(node_choices)) {
      updateSelectizeInput(session, "from_edge", 
                           choices = c("Choose edge parent" = "", node_choices), 
                           selected = isolate(input$from_edge))
      updateSelectizeInput(session, "to_edge", 
                           choices = c("Choose edge child" = "", node_choices), 
                           selected = isolate(input$to_edge))
    } else if (nrow(node_frame(rv$nodes))) {
      updateSelectizeInput(session, "from_edge", 
                           choices = c("Choose edge parent" = "", node_choices))
      updateSelectizeInput(session, "to_edge", 
                           choices = c("Choose edge child" = "", node_choices))
    } else {
      node_choices <- c("Add a node to the plot area" = "")
      updateSelectizeInput(session, "from_edge", choices = node_choices)
      updateSelectizeInput(session, "to_edge", choices = node_choices)
    }
  })
  
  # Add or Remove Edges
  observeEvent(input$edge_btn, {
    if (input$from_edge == "") {
      warnNotification('Please choose a "Parent" node')
      return()
    } else if (input$to_edge == "") {
      warnNotification('Please choose a "Child" node')
      return()
    }
    
    this_edge_key <- edge_key(input$from_edge, input$to_edge)
    if (this_edge_key %in% names(rv$edges)) {
      # Remove edge
      rv$edges <- rv$edges[setdiff(names(rv$edges), this_edge_key)]
    } else {
      # Add edge
      rv$edges[[this_edge_key]] <- list(
        from  = input$from_edge, 
        to    = input$to_edge,
        color = "black",
        angle = 0.0,
        lineT = "thin",
        lty   = "solid"
      )
    }
    debug_input(rv$edges, "rv$edges")
  })
  
  output$ui_edge_btn <- renderUI({
    if(is.null(input$from_edge) || is.null(input$to_edge)) return()
    this_edge_key <- edge_key(input$from_edge, input$to_edge)
    if (input$from_edge == "" || input$to_edge == "") {
      # Disabled button
      actionButton("edge_btn", "", icon = icon("plus"), class = "disabled")
    } else if (this_edge_key %in% names(rv$edges)) {
      # Remove edge button
      actionButton("edge_btn", "", icon = icon("minus"), class = "btn-danger")
    } else {
      # Add edge button
      actionButton("edge_btn", "", icon = icon("plus"), class = "btn-success")
    }
  })
  
  # ---- DAG Diagnostics ----
  g_dagitty <- reactive({
    req(length(rv$edges) > 0)
    edges <- edge_frame(rv$edges, rv$nodes)
    dagitty_paths <- edges %>% 
      glue::glue_data("{from}->{to};") %>% 
      glue::glue_collapse()
    dagitty_code <- glue::glue("dag {{ {dagitty_paths} }}")
    
    dagitty(dagitty_code)
  })
  
  dagitty_apply <- function(gd, nodes, exposures = NULL, outcomes = NULL, adjusted = NULL) {
    nodes <- invertNames(node_names(nodes))
    if (!is.null(exposures)) exposures(gd)      <- nodes[exposures]
    if (!is.null(outcomes))  outcomes(gd)       <- nodes[outcomes]
    if (!is.null(adjusted))  adjustedNodes(gd)  <- nodes[adjusted]
    gd
  }
  
  output$openPaths <- renderPrint({
    req(node_names(rv$nodes), input$exposureNode, input$outcomeNode)
    if (!is.null(input$exposureNode) & !is.null(input$outcomeNode)) {
      nodes <- invertNames(node_names(rv$nodes))
      gd <- g_dagitty() %>% 
        dagitty_apply(
          rv$nodes,
          exposures = input$exposureNode,
          outcomes  = input$outcomeNode,
          adjusted  = input$adjustNode
        )

      allComb <- as.data.frame(combn(names(gd), 2))

      pathData <- list(path = vector("character", 0), open = vector("character", 0))
      for (i in 1:ncol(allComb)) {
        pathResults <- paths(gd, from = allComb[1, i], to = allComb[2, i], 
                             Z = input$adjustNode %??% unname(nodes[input$adjustNode]))
        pathData$path <- c(pathData$path, pathResults$paths)
        pathData$open <- c(pathData$open, pathResults$open)
      }

      openPaths <- grep("TRUE", pathData$open)

      return(cat(pathData$path[openPaths][str_count(pathData$path[openPaths], "-") >= 1], sep = "\n"))
    } else {
      return(print(""))
    }
  })

  output$closedPaths <- renderPrint({
    req(node_names(rv$nodes), input$exposureNode, input$outcomeNode)
    if (!is.null(input$exposureNode) & !is.null(input$outcomeNode)) {
      nodes <- invertNames(node_names(rv$nodes))
      gd <- g_dagitty() %>% 
        dagitty_apply(
          rv$nodes,
          exposures = input$exposureNode,
          outcomes  = input$outcomeNode,
          adjusted  = input$adjustNode
        )

      allComb <- as.data.frame(combn(names(gd), 2))

      pathData <- list(path = vector("character", 0), open = vector("character", 0))
      for (i in 1:ncol(allComb)) {
        pathResults <- paths(gd, from = allComb[1, i], to = allComb[2, i], 
                             Z = input$adjustNode %??% unname(nodes[input$adjustNode]))
        pathData$path <- c(pathData$path, pathResults$paths)
        pathData$open <- c(pathData$open, pathResults$open)
      }

      closedPaths <- grep("FALSE", pathData$open)

      return(cat(pathData$path[closedPaths][str_count(pathData$path[closedPaths], "-") >= 1], sep = "\n"))
    } else {
      return(print(""))
    }
  })
  
  # ---- Edit Aesthetics ----
  
  # ui_edge_controls() is designed to be lapply-ed over the edge list, and
  # will create shinyInputs for each node pair.
  ui_edge_controls <- function(edge, edge_hash, inputFn, prefix_input, prefix_label, ...) {
    node_from <- invertNames(node_names(rv$nodes))[edge$from]
    node_to   <- invertNames(node_names(rv$nodes))[edge$to]
    input_name  <- paste(prefix_input, edge_hash, sep = "_")
    input_label <- paste0(prefix_label, " ", node_from, "&nbsp;&#8594; ", node_to)
    
    if (input_name %in% names(input)) {
      # Make sure current value doesn't change
      dots <- list(...)
      current_value <- intersect(names(dots), c("selected", "value"))
      dots[current_value] <- isolate(input[[input_name]])
      dots$inputId <- input_name
      dots$label <- HTML(input_label)
      do.call(inputFn, dots)
    } else {
      # Create new input
      inputFn(input_name, HTML(input_label), ...)
    }
  }
  
  output$curveAngle <- renderUI({
    req(length(rv$edges) > 0)
    purrr::imap(
      rv$edges,
      ui_edge_controls,
      inputFn = sliderInput,
      prefix_input = "angle",
      prefix_label = "Angle for",
      min = -180, max = 180, value = 0, step = 5
    )
  })
  
  output$curveColor <- renderUI({
    req(length(rv$edges) > 0)
    purrr::imap(
      rv$edges,
      ui_edge_controls,
      inputFn = textInput,
      prefix_input = "color",
      prefix_label = "Edge for",
      value = "black"
    )
  })
  
  output$curveLty <- renderUI({
    req(length(rv$edges) > 0)
    purrr::imap(
      rv$edges,
      ui_edge_controls,
      inputFn = selectInput,
      prefix_input = "lty",
      prefix_label = "Line type for",
      choices = c("solid", "dashed"),
      selected = "solid"
    )
  })
  
  output$curveThick <- renderUI({
    req(length(rv$edges) > 0)
    purrr::imap(
      rv$edges,
      ui_edge_controls,
      inputFn = selectInput,
      prefix_input = "lineT",
      prefix_label = "Line thickness",
      choices = c("ultra thin", "very thin", "thin", "semithick", "thick", "very thick", "ultra thick"),
      selected = "thin"
    )
  })
  
  # Watch edge UI inputs and update rv$edges when inputs change
  observe({
    req(length(rv$edges) > 0, grepl("^angle_", names(input)))
    edge_ui <- tibble(
      inputId = grep("^(angle|color|lty|lineT)_", names(input), value = TRUE)
    ) %>% 
      # get current value of input
      mutate(value = lapply(inputId, function(x) input[[x]])) %>% 
      tidyr::separate(inputId, into = c("var", "hash"), sep = "_") %>% 
      tidyr::spread(var, value) %>% 
      tidyr::unnest() %>% 
      split(.$hash)
    for (edge in edge_ui) {
      if (!edge$hash %in% names(rv$edges)) next
      this_edge <- edge[setdiff(names(edge), "hash")]
      for (prop in names(this_edge)) {
        rv$edges[[edge$hash]][[prop]] <- this_edge[[prop]]
      }
    }
    debug_input(rv$edges, "rv$edges after aes update")
  }, priority = -50)
  
  # ---- Render DAG ----
  # output$tikzOut <- renderUI({
  #   tikzUpdateOutput()
  #   if (!file.exists(file.path(SESSION_TEMPDIR, "DAGimageDoc.pdf"))) return(NULL)
  # 
  #   serve_file_path <- file.path(sub("www/", "", SESSION_TEMPDIR, fixed = TRUE), "DAGimageDoc.pdf")
  # 
  #   tags$iframe(
  #     style = "height:600px; width:100%",
  #     src = serve_file_path,
  #     scrolling = "auto",
  #     zoom = if (length(V(rv$g)$name) < 1) 300,
  #     seamless = "seamless"
  #   )
  # })
  
  output$tikzOut <- renderUI({
    req(length(rv$nodes), input$showPreview)
    tikzUpdateOutput()
    image_path <- file.path(SESSION_TEMPDIR, "DAGimage.png")
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
      style = "width: 100%; max-height: 600px; -o-object-fit: contain;",
      alt = "DAG"
    )
  })
  
  tikzUpdateOutput <- reactiveVal(TRUE) # Triggers PDF update when value changes
  
  edge_points_rv <- reactive({
    req(length(rv$edges) > 0)
    edge_points(rv$edges, rv$nodes)
  })
  
  dag_node_lines <- function(nodeFrame) {
    # Node frame is all points (1, 7) to (7, 1) with columns x, y, name
    nodeFrame <- nodeFrame[nodeFrame$x >= min(nodeFrame[!is.na(nodeFrame$name), ]$x) &
                             nodeFrame$x <= max(nodeFrame[!is.na(nodeFrame$name), ]$x) &
                             nodeFrame$y >= min(nodeFrame[!is.na(nodeFrame$name), ]$y) &
                             nodeFrame$y <= max(nodeFrame[!is.na(nodeFrame$name), ]$y), ]
    nodeFrame$name <- ifelse(is.na(nodeFrame$name), "~", nodeFrame$name)
    nodeFrame$nameA <- ifelse(nodeFrame$name %in% input$adjustNode, paste0(" |[module]| ", nodeFrame$name), nodeFrame$name)
    nodeLines <- vector("character", 0)
    for (i in unique(nodeFrame$y)) {
      createLines <- paste0(paste(nodeFrame[nodeFrame$y == i, ]$nameA, collapse = "&"), "\\\\")
      nodeLines <- c(nodeLines, createLines)
    }
    nodeLines <- rev(nodeLines)
    
    paste0("\\matrix(m)[matrix of nodes, row sep=2.6em, column sep=2.8em,text height=1.5ex, text depth=0.25ex, nodes={label}] {", paste(nodeLines, collapse = ""), "};")
  }

  # Re-render TeX preview
  observe({
    req(rv$nodes, input$showPreview)
    nodeFrame <- node_frame(rv$nodes)
    
    if (nrow(nodeFrame)) {
      styleZ <- "\\tikzset{ module/.style={draw, rectangle},
      label/.style={ } }"
      startZ <- "\\begin{tikzpicture}[>=latex]"
      endZ <- "\\end{tikzpicture}"
      pathZ <- "\\path[->,font=\\scriptsize,>=angle 90]"

      nodeLines <- tidyr::crossing(x = 1:7, y = 1:7) %>% 
        left_join(
          nodeFrame %>% select(x, y, name), 
          by = c("x", "y")
        ) %>% 
        dag_node_lines()

      edgeLines <- character()

      if (length(rv$edges)) {
        # edge_points_rv() is a reactive that gathers values from aesthetics UI
        # but it can be noisy, so we're debouncing to delay TeX rendering until values are constant
        edgePts <- debounce(edge_points_rv, 1000)()
        
        tikz_point <- function(x, y, x_min, y_max) {
          glue::glue("(m-{y_max - y + 1}-{x - x_min + 1})")
        }
        
        edgePts <- edgePts %>% 
          mutate(
            x_min  = min(from.x, to.x),
            y_max  = max(from.y, to.y),
            parent = tikz_point(from.x, from.y, x_min, y_max),
            child  = tikz_point(to.x, to.y, x_min, y_max),
            edgeLine = glue::glue(
              "{parent} edge [>={input$arrowShape}, bend left = {edgePts$angle}, ",
              "color = {edgePts$color},{edgePts$lineT},{edgePts$lty}] node[auto] {{$~$}} {child}"
            )
          )
        
        debug_input(select(edgePts, hash, matches("^(from|to)"), parent, child, edgeLine), "edgeLines")
        edgeLines <- edgePts$edgeLine
      }
      
      edgeLines <- paste0(pathZ, paste(edgeLines, collapse = ""), ";")

      tikzLines <- c(styleZ, startZ, nodeLines, edgeLines, endZ)
      tikzLines <- paste(tikzLines, collapse = "")

      useLib <- "\\usetikzlibrary{matrix,arrows,decorations.pathmorphing}"

      pkgs <- paste(buildUsepackage(pkg = list("tikz"), uselibrary = useLib), collapse = "\n")

      texPreview(
        obj = tikzLines,
        stem = "DAGimage",
        fileDir = SESSION_TEMPDIR,
        imgFormat = "png",
        returnType = "shiny",
        density = tex_opts$get("density"),
        keep_pdf = TRUE,
        usrPackages = pkgs,
        margin = tex_opts$get("margin"),
        cleanup = tex_opts$get("cleanup")
      )
    }
    tikzUpdateOutput(!isolate(tikzUpdateOutput()))
  }, priority = -100)
  
  # ---- Download Files ----
  # Merge tikz TeX source into main TeX file
  merge_tex_files <- function(main_file, input_file, out_file) {
    x <- readLines(main_file)
    y <- readLines(input_file)
    which_line <- grep("input{", x, fixed = TRUE)
    which_line <- intersect(which_line, grep(basename(input_file), x))
    x[which_line] <- paste(y, collapse = "\n")
    writeLines(x, out_file)
  }

  output$downloadButton <- downloadHandler(
    filename = function() {
      paste0("DAG", Sys.Date(), ifelse(input$downloadType == 1, ".RData",
        ifelse(input$downloadType == 2, ".tex",
          ifelse(input$downloadType == 3, ".png",
            ifelse(input$downloadType == 5, ".RData", ".pdf")
          )
        )
      ))
    },
    content = function(file) {
      if (input$downloadType == 1) {
        daggityCode1 <- paste0(ends(rv$g, E(rv$g))[, 1], "->", ends(rv$g, E(rv$g))[, 2])
        daggityCode1 <- paste(daggityCode1, collapse = ";")
        daggityCode2 <- paste0("dag { ", daggityCode1, " }")

        g2 <- dagitty(daggityCode2)

        exposures(g2) <- input$exposureNode
        outcomes(g2) <- input$outcomeNode
        adjustedNodes(g2) <- input$adjustNode

        dagitty_code <- g2
        save(dagitty_code, file = file)
      } else if (input$downloadType == 2) {
        merge_tex_files(
          file.path(SESSION_TEMPDIR, "DAGimageDoc.tex"),
          file.path(SESSION_TEMPDIR, "DAGimage.tex"),
          file
        )
      } else if (input$downloadType == 3) {
        myfile <- file.path(SESSION_TEMPDIR, "DAGimage.png")
        file.copy(myfile, file)
      } else if (input$downloadType == 5) {
        daggityCode1 <- paste0(ends(rv$g, E(rv$g))[, 1], "->", ends(rv$g, E(rv$g))[, 2])
        daggityCode1 <- paste(daggityCode1, collapse = ";")
        daggityCode2 <- paste0("dag { ", daggityCode1, " }")

        g2 <- dagitty(daggityCode2)

        exposures(g2) <- input$exposureNode
        outcomes(g2) <- input$outcomeNode
        adjustedNodes(g2) <- input$adjustNode

        tidy_dag <- tidy_dagitty(g2)
        save(tidy_dag, file = file)
      } else {
        myfile <- file.path(SESSION_TEMPDIR, "DAGimageDoc.pdf")
        file.copy(myfile, file)
      }
    }, contentType = NA
  )

  # ---- TeX Editor ----
  output$texEdit <- renderUI({
    if (length(V(rv$g)$name) >= 1) {
      styleZ <- "\\\\tikzset{ module/.style={draw, rectangle},
      label/.style={ } }"
      startZ <- "\\\\begin{tikzpicture}[>=latex]"
      endZ <- "\\\\end{tikzpicture}"
      pathZ <- "\\\\path[->,font=\\\\scriptsize,>=angle 90]"

      nodeFrame <- rv$pts2
      nodeFrame <- nodeFrame[nodeFrame$x >= min(nodeFrame[!is.na(nodeFrame$name), ]$x) &
        nodeFrame$x <= max(nodeFrame[!is.na(nodeFrame$name), ]$x) &
        nodeFrame$y >= min(nodeFrame[!is.na(nodeFrame$name), ]$y) &
        nodeFrame$y <= max(nodeFrame[!is.na(nodeFrame$name), ]$y), ]
      nodeFrame$name <- ifelse(is.na(nodeFrame$name), "~", nodeFrame$name)
      nodeFrame$nameA <- ifelse(nodeFrame$name %in% input$adjustNode, paste0(" |[module]| ", nodeFrame$name), nodeFrame$name)
      nodeLines <- vector("character", 0)
      for (i in unique(nodeFrame$y)) {
        createLines <- paste0(paste(nodeFrame[nodeFrame$y == i, ]$nameA, collapse = "&"), "\\\\\\\\")
        nodeLines <- c(nodeLines, createLines)
      }
      nodeLines <- rev(nodeLines)
      nodeLines2 <- nodeLines

      nodeLines <- paste0("\\\\matrix(m)[matrix of nodes, row sep=2.6em, column sep=2.8em,text height=1.5ex, text depth=0.25ex, nodes={label}] {", paste(nodeLines, collapse = ""), "};")

      edgeLines <- vector("character", 0)

      if (length(E(rv$g)) >= 1) {
        edgeFrame <- as.data.frame(ends(rv$g, E(rv$g)))
        edgeFrame$name <- paste0(edgeFrame$V1, "->", edgeFrame$V2)
        edgeFrame$angle <- edgeFrame$color <- edgeFrame$thick <- edgeFrame$type <- edgeFrame$loose <- NA
        edgeFrame$parent <- edgeFrame$child <- NA

        nodeFrame$revY <- rev(nodeFrame$y)

        for (i in 1:length(edgeFrame$name)) {
          edgeFrame$angle[i] <- ifelse(!is.null(input[[paste0("angle", edgeFrame$name[i])]]), as.numeric(input[[paste0("angle", edgeFrame$name[i])]]), 0)
          edgeFrame$color[i] <- ifelse(is.null(input[[paste0("color", edgeFrame$name[i])]]), "black", input[[paste0("color", edgeFrame$name[i])]])
          edgeFrame$thick[i] <- ifelse(is.null(input[[paste0("lineT", edgeFrame$name[i])]]), "thin", input[[paste0("lineT", edgeFrame$name[i])]])
          edgeFrame$type[i] <- ifelse(is.null(input[[paste0("lty", edgeFrame$name[i])]]), "solid", input[[paste0("lty", edgeFrame$name[i])]])
          edgeFrame$parent[i] <- paste0(
            "(m-", (nodeFrame[nodeFrame$name == edgeFrame$V1[i], ]$revY - min(nodeFrame$revY) + 1), "-",
            (nodeFrame[nodeFrame$name == edgeFrame$V1[i], ]$x - min(nodeFrame$x) + 1), ")"
          )
          edgeFrame$child[i] <- paste0(
            "(m-", (nodeFrame[nodeFrame$name == edgeFrame$V2[i], ]$revY - min(nodeFrame$revY) + 1), "-",
            (nodeFrame[nodeFrame$name == edgeFrame$V2[i], ]$x - min(nodeFrame$x) + 1), ")"
          )
          createEdge <- paste0(
            edgeFrame$parent[i], " edge [>=", input$arrowShape, ", bend left = ", edgeFrame$angle[i],
            ", color = ", edgeFrame$color[i], ",", edgeFrame$type[i], ",", edgeFrame$thick[i],
            "] node[auto] {$~$} ", edgeFrame$child[i], " "
          )
          edgeLines <- c(edgeLines, createEdge)
        }
      }

      edgeLines <- paste0(pathZ, paste(edgeLines, collapse = ""), ";")

      allLines <- c(styleZ, startZ, nodeLines, edgeLines, endZ)

      tikzTemp <- paste(allLines, collapse = "")
    } else {
      startZ <- "\\\\begin{tikzpicture}[>=latex]"
      endZ <- "\\\\end{tikzpicture}"

      allLines <- c(startZ, endZ)

      tikzTemp <- paste(allLines, collapse = "")
    }
    aceEditor("texChange", mode = "latex", value = paste(allLines, collapse = "\n"), theme = "cobalt")
  })

  output$tikzOutNew <- renderUI({
    tikzTemp <- input$texChange

    useLib <- "\\usetikzlibrary{matrix,arrows,decorations.pathmorphing}"

    pkgs <- paste(buildUsepackage(pkg = list("tikz"), uselibrary = useLib), collapse = "\n")

    texPreview(
      obj = tikzTemp,
      stem = "DAGimageEdit",
      fileDir = SESSION_TEMPDIR,
      imgFormat = "png",
      returnType = "shiny",
      density = tex_opts$get("density"),
      keep_pdf = TRUE,
      usrPackages = pkgs,
      margin = tex_opts$get("margin"),
      cleanup = tex_opts$get("cleanup")
    )
    
    session_id <- sub("www/", "", SESSION_TEMPDIR, fixed = TRUE)

    return(tags$iframe(
      style = "height:560px; width:100%", src = file.path(session_id, "DAGimageEditDoc.pdf"),
      scrolling = "no", seamless = "seamless"
    ))
  })

  output$downloadButton2 <- downloadHandler(
    filename = function() {
      paste0("DAG", Sys.Date(), ifelse(input$downloadType2 == 1, ".tex",
        ifelse(input$downloadType2 == 2, ".png", ".pdf")
      ))
    },
    content = function(file) {
      if (input$downloadType2 == 1) {
        merge_tex_files(
          file.path(SESSION_TEMPDIR, "DAGimageEditDoc.tex"),
          file.path(SESSION_TEMPDIR, "DAGimageEdit.tex"),
          file
        )
      } else if (input$downloadType2 == 2) {
        myfile <- file.path(SESSION_TEMPDIR, "DAGimageEdit.png")
        file.copy(myfile, file)
      } else {
        myfile <- file.path(SESSION_TEMPDIR, "DAGimageEditDoc.pdf")
        file.copy(myfile, file)
      }
    }, contentType = NA
  )
}

# Run the application
shinyApp(ui = ui, server = server)
