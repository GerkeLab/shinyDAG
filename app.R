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

tex_opts$set(list(
  density = 1200,
  margin = list(left = 0, top = 0, right = 0, bottom = 0),
  cleanup = c("aux", "log")
))


# Functions ---------------------------------------------------------------

DEBUG <- getOption("shinydag.debug", FALSE)
debug_input <- function(x, x_name = NULL) {
  if (!isTRUE(DEBUG)) return()
  
  if (length(x) == 1 && !is.list(x)) {
    cat("\n", if (!is.null(x_name)) paste0(x_name, ":"), if (length(names(x))) names(x), "-", x)
  } else {
    x <- tibble::enframe(x)
    cat("", if (!is.null(x_name)) paste0(x_name, ":"), knitr::kable(x), sep = "\n")
  }
}
debug_line <- function(...) {
  if (!isTRUE(DEBUG)) return()
  cli::cat_line(...)
}


buildUsepackage <- if (length(find("build_usepackage"))) texPreview::build_usepackage else texPreview::buildUsepackage
`%||%` <- function(x, y) if (is.null(x)) y else x

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
    fluidRow(
      # ---- Box: DAG ----
      box(
        title = "shinyDAG",
        column(12, align = "center", uiOutput("tikzOut")),
        selectInput("downloadType", "Type of download",
          choices = list("PDF" = 4, "PNG" = 3, "LaTeX TikZ" = 2, "dagitty R object" = 1, "ggdag R object" = 5)
        ),
        downloadButton("downloadButton"),
        br(), br(),
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
            "#node_delete { margin-top: 25px; color: #FFF }",
            "@media (min-width: 768px) { #node_delete { margin-left: -25px; } }"
          ),
          fluidRow(
            column(10, 
              searchInput("nodeLabel", label = "Add a Node", value = "", placeholder = NULL,
                          btnSearch = icon("check"), btnReset = icon("remove"), width = "100%")
            ),
            column(2, uiOutput("node_ui_remove"))
          ),
          uiOutput("nodeListButtons"),
          # checkboxInput("clickType", "Click to remove a node", value = FALSE),
          plotOutput("clickPad", click = "pad_click"),
          fluidRow(
            column(6, selectizeInput("fromEdge2", "Parent Node", choices = c("Add a node to the plot area" = ""))),
            column(6, selectizeInput("toEdge2", "Child Node", choices = c("Add a node to the plot area" = "")))
          ),
          actionButton("edgeButton1", "Add edge!"),
          actionButton("edgeButton2", "Remove edge!"),
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
    pts2 = data_frame(x = rep(1:7, each = 7), y = rep(1:7, 7), name = rep(NA, 49))
  )
  
  node_list_btn_last_state <- c()
  
  # rv$edges is a named list, e.g. for A -> B:
  # rv$edges["A_B"] = list(from = "A", to = "B")
  
  # rv$nodes is a named list where name is a hash
  # rv$nodes$abcdefg = list(name, x, y)
  
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
    if (!length(nodes)) return(data_frame())
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
        new_node_state <- setNames(FALSE, node_btn_id(new_node_hash))
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
      tags$p(tags$strong("Select a Node")),
      node_list_buttons
    )
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
    updateSearchInput(session, "nodeLabel", value = unname(s_node_name), label = "Edit Node")
  })
  
  # Selecting existing node label enables node delete button
  output$node_ui_remove <- renderUI({
    req(node_list_btn_sel())
    if (node_btn_sel_has_point()) {
      actionButton("node_delete", "", icon = icon("eraser"), class = "btn-warning", alt = "Clear Node from DAG")
    } else {
      actionButton("node_delete", "", icon = icon("trash"), class = "btn-danger", alt = "Delete Node")
    }
  })
  
  # Erase or delete button?
  node_btn_sel_has_point <- reactive({
    req(node_list_btn_sel())
    node_btn_hash <- node_btn_get_hash(node_list_btn_sel())
    !is.na(rv$nodes[[node_btn_hash]]$x)
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
    debug_input(rv$nodes, "rv$nodes")
    debug_input(node_list_btn_state(), "node_list_btn_state()")
  })
  
  # Clear nodeLabel input when no node buttons selected
  observe({
    req(!any(node_list_btn_state()))
    # Clear node input
    updateSearchInput(session, "nodeLabel", value = "", label = "Add a Node")
  })

  # ---- Click Pad ----
  # Add or move point on clickPad
  observeEvent(input$pad_click, {
    cli::cat_line("Click!")
    
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
    
    debug_line("Rebuilding graph")
    rv$gg <- make_empty_graph() + 
      node_vertices(rv$nodes)
    
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
      text(rv_pts$x, rv_pts$y, labels = rv_pts$name, cex = 2, col = rv_pts$color)
    } else {
      plot(NA, NA, xlim = c(1, 7), ylim = c(1, 7), bty = "n", xaxt = "n", yaxt = "n", ylab = "", xlab = "", xaxs = "i")
      grid()
    }
  })

  # ---- Node - Options ----
  update_node_options <- function(nodes, inputId, updateFn, none_choice = TRUE, ...) {
    available_choices <- c("None" = if (none_choice) "", node_names(nodes))
    s_choice <- intersect(
      c(isolate(input[[inputId]]), if (none_choice) ""), 
      available_choices
    )
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
  })
  
  observe({
    update_node_options(rv$nodes, "exposureNode", 
                        updateRadioButtons, inline = TRUE)
  })

  observe({
    update_node_options(rv$nodes, "outcomeNode", 
                        updateRadioButtons, inline = TRUE)
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
  # TODO: rename input$fromEdge2 and toEdge2 to fromEdge/toEdge
  
  # Update Parent/Child node selection for edges
  observe({
    node_choices <- node_names(rv$nodes)
    if (length(node_choices)) {
      node_choices <- c("---" = "", node_choices)
      updateSelectizeInput(session, "fromEdge2", choices = node_choices, selected = isolate(input$fromEdge2))
      updateSelectizeInput(session, "toEdge2", choices = node_choices, selected = isolate(input$toEdge2))
    } else {
      node_choices <- c("Add a node to the plot area" = "")
      updateSelectizeInput(session, "fromEdge2", choices = node_choices)
      updateSelectizeInput(session, "toEdge2", choices = node_choices)
    }
  })

  edge_key <- function(x, y) paste(x, y, sep = "_")
  
  # add/remove edges to DAG
  observeEvent(input$edgeButton1, {
    if (input$fromEdge2 %in% V(rv$g)$name & input$toEdge2 %in% V(rv$g)$name) {
      rv$g <- rv$g %>%
        add_edges(c(input$fromEdge2, input$toEdge2)) %>%
        set_edge_attr("color", value = "black")
      
      
      rv$edges[[edge_key(input$fromEdge2, input$toEdge2)]] <- list(
        from = input$fromEdge2, to = input$toEdge2
      )
    }
  })

  observeEvent(input$edgeButton2, {
    if (input$fromEdge2 %in% V(rv$g)$name & input$toEdge2 %in% V(rv$g)$name) {
      rv$g <- rv$g %>%
        delete_edges(paste0(input$fromEdge2, "|", input$toEdge2))
      
      rv$edges <- rv$edges[setdiff(names(rv$edges), edge_key(input$fromEdge2, input$toEdge2))]
    }
  })

  # ---- DAG Diagnostics ----
  output$adjustSets <- renderPrint({
    if (!is.null(input$exposureNode) & !is.null(input$outcomeNode)) {
      daggityCode1 <- paste0(ends(rv$g, E(rv$g))[, 1], "->", ends(rv$g, E(rv$g))[, 2])
      daggityCode1 <- paste(daggityCode1, collapse = ";")
      daggityCode2 <- paste0("dag { ", daggityCode1, " }")

      g2 <- dagitty(daggityCode2)

      exposures(g2) <- input$exposureNode
      outcomes(g2) <- input$outcomeNode

      adjustResults <- adjustmentSets(g2)
      return(adjustResults)
    } else {
      return(print("Please indicate exposure and outcome"))
    }
  })

  output$condInd <- renderPrint({
    daggityCode1 <- paste0(ends(rv$g, E(rv$g))[, 1], "->", ends(rv$g, E(rv$g))[, 2])
    daggityCode1 <- paste(daggityCode1, collapse = ";")
    daggityCode2 <- paste0("dag { ", daggityCode1, " }")

    g2 <- dagitty(daggityCode2)

    exposures(g2) <- input$exposureNode
    outcomes(g2) <- input$outcomeNode
    adjustedNodes(g2) <- input$adjustNode

    test <- impliedConditionalIndependencies(g2)

    return_list <- vector("character", 0)
    for (i in 1:length(test)) {
      return_list <- c(return_list, paste0(test[[i]]$X, " is independent of ", test[[i]]$Y, " given: ", paste0(test[[i]]$Z, collapse = " and ")))
    }
    return(cat(return_list, sep = "\n")) # } else{return(print(""))}
  })

  output$openPaths <- renderPrint({
    if (!is.null(input$exposureNode) & !is.null(input$outcomeNode)) {
      daggityCode1 <- paste0(ends(rv$g, E(rv$g))[, 1], "->", ends(rv$g, E(rv$g))[, 2])
      daggityCode1 <- paste(daggityCode1, collapse = ";")
      daggityCode2 <- paste0("dag { ", daggityCode1, " }")

      g2 <- dagitty(daggityCode2)

      exposures(g2) <- input$exposureNode
      outcomes(g2) <- input$outcomeNode
      adjustedNodes(g2) <- input$adjustNode

      allComb <- as.data.frame(combn(names(g2), 2))

      pathData <- list(path = vector("character", 0), open = vector("character", 0))
      for (i in 1:ncol(allComb)) {
        pathResults <- paths(g2, from = allComb[1, i], to = allComb[2, i], Z = input$adjustNode)
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
    if (!is.null(input$exposureNode) & !is.null(input$outcomeNode)) {
      daggityCode1 <- paste0(ends(rv$g, E(rv$g))[, 1], "->", ends(rv$g, E(rv$g))[, 2])
      daggityCode1 <- paste(daggityCode1, collapse = ";")
      daggityCode2 <- paste0("dag { ", daggityCode1, " }")

      g2 <- dagitty(daggityCode2)

      exposures(g2) <- input$exposureNode
      outcomes(g2) <- input$outcomeNode
      adjustedNodes(g2) <- input$adjustNode

      allComb <- as.data.frame(combn(names(g2), 2))

      pathData <- list(path = vector("character", 0), open = vector("character", 0))
      for (i in 1:ncol(allComb)) {
        pathResults <- paths(g2, from = allComb[1, i], to = allComb[2, i], Z = input$adjustNode)
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
  ui_edge_controls <- function(x, inputFn, prefix_input, prefix_label, ...) {
    from_to <- paste0(x$from, "->", x$to)
    input_name <- paste0(prefix_input, from_to)
    input_label <- paste0(prefix_label, " ", from_to)
    
    if (input_name %in% names(input)) {
      # Make sure current value doesn't change
      dots <- list(...)
      current_value <- intersect(names(dots), c("selected", "value"))
      dots[current_value] <- isolate(input[[input_name]])
      dots$inputId <- input_name
      dots$label <- input_label
      do.call(inputFn, dots)
    } else {
      # Create new input
      inputFn(input_name, input_label, ...)
    }
  }
  
  output$curveAngle <- renderUI({
    req(rv$edges)
    lapply(
      rv$edges,
      ui_edge_controls,
      inputFn = sliderInput,
      prefix_input = "angle",
      prefix_label = "Angle for",
      min = -180, max = 180, value = 0, step = 5
    )
  })

  output$curveColor <- renderUI({
    req(rv$edges)
    lapply(
      rv$edges,
      ui_edge_controls,
      inputFn = textInput,
      prefix_input = "color",
      prefix_label = "Edge for",
      value = "black"
    )
  })

  output$curveLty <- renderUI({
    req(rv$edges)
    lapply(
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
    req(rv$edges)
    lapply(
      rv$edges,
      ui_edge_controls,
      inputFn = selectInput,
      prefix_input = "lineT",
      prefix_label = "Line thickness",
      choices = c("ultra thin", "very thin", "thin", "semithick", "thick", "very thick", "ultra thick"),
      selected = "thin"
    )
  })
  
  # ---- Render DAG ----
  output$tikzOut <- renderUI({
    tikzUpdateOutput()
    if (!file.exists(file.path(SESSION_TEMPDIR, "DAGimageDoc.pdf"))) return(NULL)
    
    serve_file_path <- file.path(sub("www/", "", SESSION_TEMPDIR, fixed = TRUE), "DAGimageDoc.pdf")
    
    tags$iframe(
      style = "height:600px; width:100%", 
      src = serve_file_path,
      scrolling = "auto",
      zoom = if (length(V(rv$g)$name) < 1) 300,
      seamless = "seamless"
    )
  })
  
  tikzUpdateOutput <- reactiveVal(TRUE) # Triggers PDF update when value changes
  
  edge_frame <- reactive({
    req(ends(rv$g, E(rv$g)))
    as_data_frame(ends(rv$g, E(rv$g))) %>% 
      mutate(
        name   = paste0(V1, "->", V2),
        angle  = vapply(paste0("angle", name), function(n) input[[n]] %||%       0, double(1)),
        color  = vapply(paste0("color", name), function(n) input[[n]] %||% "black", character(1)),
        thick  = vapply(paste0("lineT", name), function(n) input[[n]] %||%  "thin", character(1)),
        type   = vapply(paste0("lty", name),   function(n) input[[n]] %||% "solid", character(1)),
        parent = NA,
        child  = NA
      )
  })

  # Re-render TeX preview
  observe({
    if (length(V(rv$g)$name) >= 1) {
      styleZ <- "\\tikzset{ module/.style={draw, rectangle},
      label/.style={ } }"
      startZ <- "\\begin{tikzpicture}[>=latex]"
      endZ <- "\\end{tikzpicture}"
      pathZ <- "\\path[->,font=\\scriptsize,>=angle 90]"

      nodeFrame <- rv$pts2
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
      nodeLines2 <- nodeLines

      nodeLines <- paste0("\\matrix(m)[matrix of nodes, row sep=2.6em, column sep=2.8em,text height=1.5ex, text depth=0.25ex, nodes={label}] {", paste(nodeLines, collapse = ""), "};")

      edgeLines <- vector("character", 0)

      if (length(E(rv$g)) >= 1) {
        # edge_frame() is a reactive that gathers values from aesthetics UI
        # but it can be noisy, so we're debouncing to delay TeX rendering until values are constant
        edgeFrame <- debounce(edge_frame, 1000)()

        nodeFrame$revY <- rev(nodeFrame$y)

        for (i in seq_len(nrow(edgeFrame))) {
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

      useLib <- "\\usetikzlibrary{matrix,arrows,decorations.pathmorphing}"

      pkgs <- paste(buildUsepackage(pkg = list("tikz"), uselibrary = useLib), collapse = "\n")

      texPreview(
        obj = tikzTemp,
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
  })
  
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
