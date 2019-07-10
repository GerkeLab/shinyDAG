
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
  
  # ---- Bookmarking ----
  setBookmarkExclude(c("node_list_node_delete", "node_list_node_erase", "node_list_node_add"))
  
  onBookmark(function(state) {
    state$values$rvn <- list()
    state$values$rvn$nodes <- rvn$nodes
    state$values$rve <- list()
    state$values$rve$edges <- rve$edges
    
    # Store outcome/exposure/adjust node selections
    state$values$sel <- list(
      exposureNode = input$exposureNode,
      outcomeNode = input$outcomeNode,
      adjustNode = input$adjustNode
    )
  })
  
  onBookmarked(function(url) {
    message("bookmark: ", url)
    showBookmarkUrlModal(url)
  })
  
  onRestore(function(state) {
    showModal(modalDialog(
      title = NULL,
      easyClose = FALSE,
      footer = NULL,
      tags$p(class = "text-center", "Loading your ShinyDag workspace, please wait."),
      tags$div(class = "gerkelab-spinner")
    ))
    if (isTRUE(getOption("shinydag.debug", FALSE))) {
      names(state$values) %>%
        purrr::set_names() %>%
        purrr::map(~ state$values[[.]]) %>%
        purrr::compact() %>%
        purrr::iwalk(~ debug_input(.x, paste0("state$values$", .y)))
    }
    rvn$nodes <- state$values$rvn$nodes
    rve$edges <- state$values$rve$edges
  })
  
  onRestored(function(state) {
    removeModal()
    updateSelectInput(session, "exposureNode", selected = state$values$sel$exposureNode)
    updateSelectInput(session, "outcomeNode", selected = state$values$sel$outcomeNode)
    updateSelectizeInput(session, "adjustNode", selected = state$values$sel$adjustNode)
  })
  
  # ---- Reactive Values ----
  rve <- reactiveValues(edges = list())
  rvn <- reactiveValues(nodes = list())
  
  
  # rve$edges is a named list, e.g. for hash(A) -> hash(B):
  # rve$edges[edge_key(hash(A), hash(B))] = list(from = hash(A), to = hash(B))
  
  # rvn$nodes is a named list where name is a hash
  # rvn$nodes$abcdefg = list(name, x, y)

  # ---- Node Controls ----
  node_btn_id <- function(node_hash) paste0("node_toggle_", node_hash)
  node_btn_get_hash <- function(node_btn_id) sub("node_toggle_", "", node_btn_id, fixed = TRUE)
  
  node_list_buttons_redraw <- reactiveVal(Sys.time())
  node_list_node_is_new <- reactiveVal(FALSE)
  node_list_selected_node <- reactive({ node_parent(rvn$nodes) })
  node_list_selected_child <- reactive({ node_child(rvn$nodes) })
  
  # debug selected nodes
  observe({
    debug_input(node_list_selected_node(), "node_list_selected_node")
    debug_input(node_list_selected_child(), "node_list_selected_child")
  })
  
  # Handle add node button, creates new node and sets focus
  observeEvent(input$node_list_node_add, {
    new_node_hash <- digest::digest(Sys.time())
    rvn$nodes <- node_new(rvn$nodes, new_node_hash, "new node") %>% 
      node_set_attribute(new_node_hash, "parent")
    node_list_buttons_redraw(Sys.time())
    node_list_node_is_new(TRUE)
  })
  
  # Show, hide or update node name text input
  observe({
    if (is.null(node_list_selected_node())) {
      shinyjs::hide("node_list_node_name_container")
      return()
    } 
    
    s_node_selected <- node_list_selected_node()
    
    # Selected node already exists, update UI
    shinyjs::show("node_list_node_name_container")
    shinyjs::runjs("set_input_focus('node_list_node_name')")
    s_node_name <- node_name_from_hash(isolate(rvn$nodes), s_node_selected)
    if (isolate(node_list_node_is_new())) {
      node_list_node_is_new(FALSE)
      updateTextInput(session, "node_list_node_name", value = "", placeholder = "Enter Node Name")
    } else {
      updateTextInput(
        session, 
        "node_list_node_name", 
        value = unname(s_node_name)
      )
    }
  }, priority = 1000)
  
  # Handle node name text input
  observeEvent(input$node_list_node_name, {
    # node_name_debounced <- debounce(node_list_name, 500)
    # debug_input(node_label_debounced(), "node_label_debounced")
    req(node_list_selected_node(), input$node_list_node_name != "")
    rvn$nodes <- node_update(rvn$nodes, node_list_selected_node(), input$node_list_node_name)
  }, priority = -1000)
  
  # Show editing buttons when appropriate
  observe({
    I("toggle edit buttons")
    if (is.null(node_list_selected_node()) || !length(rvn$nodes)) {
      # no node selected, can only add a new node
      shinyjs::hide("node_list_node_delete")
    } else {
      # can now delete any selected node
      shinyjs::show("node_list_node_delete")
    }
  })
  
  # Action: delete node
  observeEvent(input$node_list_node_delete, {
    # Remove node
    rvn$nodes[[node_list_selected_node()]] <- NULL
    
    # Remove any edges
    edges_with_node <- rve$edges %>% 
      purrr::keep(~ node_list_selected_node() %in% c(.$from, .$to)) %>% 
      names()
    
    if (length(edges_with_node)) rve$edges[edges_with_node] <- NULL
    
    shinyjs::hide("node_list_node_name_container")
    shinyjs::hide("node_list_node_delete")
  })
  
  output$node_list_helptext <- renderUI({
    s_node <- node_list_selected_node()
    no_nodes <- length(rvn$nodes) == 0
    not_enough_nodes <- length(rvn$nodes) < 2
    no_node_selected <- !no_nodes && is.null(s_node)
    no_dag_nodes <- !no_nodes && length(nodes_in_dag(rvn$nodes)) == 0
    not_enough_dag_nodes <- !no_dag_nodes && length(nodes_in_dag(rvn$nodes)) < 2
    node_in_dag <- !no_dag_nodes && s_node %in% nodes_in_dag(rvn$nodes)
    
    if (no_nodes) {
      helpText(
        "Use the", icon("plus"), "button above to add a node",
        "to your shinyDAG workspace"
      )
    } else if (not_enough_nodes) {
      helpText("Add another node to your shinyDAG workspace")
    } else if (no_dag_nodes) {
      helpText("Drag a node from the staging area into the DAG or click its label to edit")
    } else if (not_enough_dag_nodes) {
      helpText("Drag another node from the staging area into the DAG")
    } else if (input$clickpad_click_action == "parent") {
      helpText("Click on a node label to activate as causal node or to edit its label")
    } else if (input$clickpad_click_action == "child") {
      helpText(
        "Click on a node label to draw or remove a causal arrow from", 
        tags$strong(node_name_from_hash(rvn$nodes, node_list_selected_node()))
      )
    }
  })
  
  # ---- Click Pad ----
  req_nodes <- function() {
    if (!length(rvn$nodes)) {
      cat("\n No Nodes!")
      edge_helptext("Please add a node to the DAG first.")
      FALSE
    } else TRUE
  }
  
  edge_helptext <- function(inner, tag = "div", class = "help-block text-danger alert-edge") {
    edge_helptext_trigger(Sys.time())
    edge_helptext_feedback(list(class = class, inner = inner, tag = tag))
  }
  
  edge_normal_help_html <- list(
    inner = "Double-click on a node to set parent node. Single-click to set child node.",
    class = "help-block",
    tag = "p"
  )
  edge_helptext_trigger <- reactiveVal(Sys.time())
  edge_helptext_feedback <- reactiveVal(NULL)
  
  output$edge_list_helptext <- renderUI({
    debug_input(isolate(edge_helptext_feedback()), "edge_helptext_feedback")
    
    edge_helptext_trigger()
    
    if (!is.null(isolate(edge_helptext_feedback()))) {
      invalidateLater(4800)
    } 
    
    html <- isolate(edge_helptext_feedback()) %||% edge_normal_help_html
    edge_helptext_feedback(NULL)
    tag(html$tag, list(class = html$class, html$inner))
  })
  
  clickpad_new_locations <- callModule(
    clickpad, "clickpad", 
    nodes = reactive(rvn$nodes),
    edges = reactive(rve$edges)
  )
  
  observe({
    req(clickpad_new_locations())
    
    new <- clickpad_new_locations()
    
    rvn$nodes <- node_update(rvn$nodes, new$hash, x = unname(new$x), y = unname(new$y))
  })
  
  # Watch clickpad click events
  observe({
    clicked_annotation <- event_data("plotly_clickannotation", source = "clickpad", priority = "event")
    req(clicked_annotation[["_input"]]$node_hash)
    
    click_action = isolate(input$clickpad_click_action)
    clicked_hash = clicked_annotation[["_input"]]$node_hash
    
    nodes <- isolate(rvn$nodes)
    
    s_node_parent <- node_parent(nodes)
    s_node_child <- node_child(nodes)
    has_edge <- edge_exists(isolate(rve$edges), s_node_parent, s_node_child %||% clicked_hash)
    
    if (click_action == "parent") {
      # toggle clicked node as parent node
      update_button <- nodes[[clicked_hash]]$x >= 0 &&
        nodes %>% purrr::map_dbl("x") %>% { sum(. >= 0) > 1 }
      
      if (is.null(s_node_parent)) {
        nodes <- node_set_attribute(nodes, clicked_hash, "parent")
      } else if (clicked_hash == s_node_parent) {
        update_button <- FALSE
        nodes <- node_unset_attribute(nodes, clicked_hash, c("parent", "child"))
      } else {
        nodes <- node_set_attribute(nodes, clicked_hash, "parent")
        nodes <- node_unset_attribute(nodes, clicked_hash, "child")
      }
      if (update_button) updateRadioSwitchButtons("clickpad_click_action", "child")
    } else if (click_action == "child") {
      # toggle clicked node as child node
      if (!is.null(s_node_parent) && s_node_parent == clicked_hash) {
        # Can't add edges to self
        rvn$nodes <- node_unset_attribute(nodes, names(nodes), c("parent", "child"))
        updateRadioSwitchButtons("clickpad_click_action", "parent")
        return()
      } else if (has_edge) {
        # Clicked on child node that already has edge, will be removing edge
        nodes <- node_unset_attribute(nodes, clicked_hash, "child")
      } else if (nodes[[clicked_hash]]$x < 0) {
        showNotification(
          "Edges can only be drawn between nodes that are in the DAG area.",
          duration = 5,
          type = "error"
        )
        return()
      } else {
        nodes <- node_set_attribute(nodes, clicked_hash, "child")
      }
      rve$edges <- edge_toggle(isolate(rve$edges), s_node_parent, clicked_hash)
    }
    rvn$nodes <- nodes
  })
  
  observe({
    reset_clickpad_action <- function() {
      updateRadioSwitchButtons("clickpad_click_action", "parent")
      invisible()
    }
    
    if (length(rvn$nodes) < 2) return(reset_clickpad_action())
    
    dag_has_two_nodes <- rvn$nodes %>% purrr::map_dbl("x") %>% { sum(. >= 0) > 1 }
    if (!dag_has_two_nodes) return(reset_clickpad_action())
    
    if (!is.null(node_list_selected_node())) {
      if (rvn$nodes[[node_list_selected_node()]]$x < 0) {
        reset_clickpad_action()
      }
    }
  })
  
  # Don't allow clickpad edge adding unless node conditions are met
  observeEvent(input$clickpad_click_action, {
    req(input$clickpad_click_action == "child")
    valid <- FALSE
    if (length(rvn$nodes) < 2) {
      showNotification("Please add at least 2 nodes to your DAG workspace first.", duration = 5)
    } else if (rvn$nodes %>% purrr::keep(~ .$x >= 0) %>% length() < 2) {
      showNotification("Please drag at least 2 nodes into the DAG area first.", duration = 5)
    } else if (is.null(node_list_selected_node())) {
      showNotification("A parent node must be selected first", duration = 5)
    } else if (!length(nodes_in_dag(rvn$nodes))) {
      showNotification(
        "Please add a node to the DAG by dragging it out of the staging area.", 
        duration = 5
      )
    } else {
      valid <- TRUE
    }
    if (!valid) updateRadioSwitchButtons("clickpad_click_action", "parent")
  })
  
  # ---- Node - Options ----
  update_node_options <- function(
    nodes,
    inputId,
    updateFn,
    none_choice = TRUE,
    ...,
    toggle = TRUE
  ) {
    available_choices <- c("None" = "", node_names(nodes))
    if (!none_choice) available_choices <- available_choices[-1]
    s_choice <- intersect(isolate(input[[inputId]]), available_choices)
    if (!length(s_choice) && none_choice) s_choice <- ""
    
    if (toggle) {
      shinyjs::toggleState(
        inputId,
        condition = length(available_choices) - as.integer(none_choice) > 0
      )
    }
    
    updateFn(
      session,
      inputId,
      choices = available_choices,
      selected = s_choice,
      ...
    )
  }
  
  observe({
    update_node_options(
      rvn$nodes %>% purrr::keep(~ .$x >= 0), 
      "adjustNode", 
      updateSelectizeInput, 
      toggle = TRUE
    )
    update_node_options(
      rvn$nodes %>% purrr::keep(~ .$x >= 0), 
      "exposureNode", 
      updateSelectInput, 
      toggle = TRUE
    )
    update_node_options(
      rvn$nodes %>% purrr::keep(~ .$x >= 0), 
      "outcomeNode", 
      updateSelectInput, 
      toggle = TRUE
    )
  })
  
  observeEvent(input$exposureNode, {
    nodes <- isolate(rvn$nodes)
    if (input$exposureNode == "") {
      rvn$nodes <- node_unset_attribute(nodes, names(nodes), "exposure")
    } else if (input$exposureNode == input$outcomeNode) {
      updateSelectInput(session, "outcomeNode", selected = "")
      rvn$nodes <- node_unset_attribute(nodes, names(nodes), "outcome")
    } else {
      rvn$nodes <- node_set_attribute(nodes, input$exposureNode, "exposure")
    }
  })
  
  observeEvent(input$outcomeNode, {
    nodes <- isolate(rvn$nodes)
    if (input$outcomeNode == "") {
      rvn$nodes <- node_unset_attribute(nodes, names(nodes), "outcome")
    } else if (input$outcomeNode == input$exposureNode) {
      updateSelectInput(session, "exposureNode", selected = "")
      rvn$nodes <- node_unset_attribute(nodes, names(nodes), "exposure")
    } else {
      rvn$nodes <- node_set_attribute(nodes, input$outcomeNode, "outcome")
    }
  })
  
  observeEvent(input$adjustNode, {
    nodes <- isolate(rvn$nodes)
    if (is.null(input$adjustNode)) return()
    s_adjust <- input$adjustNode
    rvn$nodes <- if (length(s_adjust) == 1 && s_adjust == "") {
      node_unset_attribute(nodes, names(node), "adjusted")
    } else {
      node_set_attribute(nodes, s_adjust, "adjusted")
    }
  })
  
  output$adjustText <- renderText({
    if (is.null(input$exposureNode) & is.null(input$outcomeNode)) {
      paste0("Minimal sufficient adjustment sets")
    } else {
      paste0(
        "Minimal sufficient adjustment set(s) to estimate the effect of ",
        input$exposureNode,
        " on ",
        input$outcomeNode
      )
    }
  })
  
  # ---- DAG Diagnostics ----
  g_dagitty <- reactive({
    req(length(rve$edges) > 0)
    edges <- edge_frame(rve$edges, rvn$nodes)
    dagitty_paths <- edges %>%
      glue::glue_data("{from}->{to};") %>%
      glue::glue_collapse()
    dagitty_code <- glue::glue("dag {{ {dagitty_paths} }}")
    debug_input(dagitty_code, "daggity_code")
    
    dagitty(dagitty_code)
  })
  
  dagitty_apply <- function(gd, nodes, exposures = NULL, outcomes = NULL, adjusted = NULL) {
    nodes <- invertNames(node_names(nodes))
    if (!is.null(exposures)) exposures(gd) <- nodes[exposures]
    if (!is.null(outcomes)) outcomes(gd) <- nodes[outcomes]
    if (!is.null(adjusted)) adjustedNodes(gd) <- nodes[adjusted]
    gd
  }
  
  dagitty_open_exp_outcome_paths <- reactive({
    req(rve$edges)
    
    # need both exposure and outcome node
    requires_nodes <- c("Exposure" = input$exposureNode, "Outcome" = input$outcomeNode)
    missing_nodes <- names(requires_nodes[grepl("^$", requires_nodes)])
    validate(
      need(
        length(missing_nodes) == 0,
        glue::glue("Please choose {str_and(missing_nodes)} {str_plural(missing_nodes, 'node')}")
      )
    )
    
    nodes <- invertNames(node_names(rvn$nodes))
    gd <- g_dagitty() %>%
      dagitty_apply(
        rvn$nodes,
        exposures = input$exposureNode,
        outcomes = input$outcomeNode,
        adjusted = input$adjustNode
      )
    
    exp_outcome_paths <- paths(
      gd,
      Z = input$adjustNode %??% unname(nodes[input$adjustNode])
    )
    
    exp_outcome_paths$paths[as.logical(exp_outcome_paths$open)]
  })
  
  dagitty_format_paths <- function(paths) {
    HTML(paste0(
      "<pre><code>",
      paste(trimws(paths), collapse = "
"),
      "\n</code></pre>"
    ))
  }
  
  output$openExpOutcomePaths <- renderUI({
    validate(need(length(rve$edges) > 0, "Please add at least one edge"))
    
    open_paths <- dagitty_open_exp_outcome_paths()
    
    if (length(open_paths)) {
      tagList(
        h5("Open associations between exposure and outcome"),
        dagitty_format_paths(open_paths)
      )
    } else {
      tagList(
        helpText("No open associations between exposure and outcome.")
      )
    }
  })
  
  # ---- Edit Aesthetics ----
  
  # The input for angles (here for easy refactoring or future changes)
  selectDegree <- function(inputId, label = "Degree", min = -180, max = 180, by = 15, value = 0, ...) {
    sliderInput(inputId, label = label, min = min, max = max, value = value, step = by)
  }
  
  # These helper functions build up the Edge UI elements.
  # * ui_edge_controls() builds an individual UI control element. These elements
  #   are re-rendered whenever the tab is opened, so this function finds the
  #   current value of the input and uses that instead of the value declared
  #   in the definition in ui_edge_controls_row(). This function also isolates
  #   the edge control UI from other changes in nodes, etc, because they happen
  #   on different screens.
  #
  # * ui_edge_controls_row() creates the entire row of UI elements for a given
  #   edge. This function is where the UI inputs are initially defined.
  
  ui_edge_controls <- function(edge_hash, inputFn, prefix_input, label, ...) {
    current_value_arg_name <- intersect(names(list(...)), c("selected", "value"))
    if (!length(current_value_arg_name)) {
      stop("Must specifiy `selected` or `value` when specifying edge UI controls")
    }
    input_name <- paste(prefix_input, edge_hash, sep = "_")
    input_label <- label
    
    if (input_name %in% names(isolate(input))) {
      # Make sure current value doesn't change
      dots <- list(...)
      dots[current_value_arg_name] <- paste(isolate(input[[input_name]]))
      dots$inputId <- input_name
      dots$label <- HTML(input_label)
      do.call(inputFn, dots)
    } else {
      # Create new input
      inputFn(input_name, HTML(input_label), ...)
    }
  }
  
  ui_edge_controls_row <- function(hash, from, to, ...) {
    col_4 <- function(x) {
      tags$div(class = "col-sm-6 col-md-3", style = "min-height: 80px", x)
    }
    title_row <- function(x) tags$div(class = "col-xs-12", tags$h3(x))
    edge_label <- paste0(from, "&nbsp;&#8594; ", to)
    
    tagList(
      fluidRow(
        title_row(HTML(edge_label))
      ),
      fluidRow(
        # Edge Curve Angle
        col_4(ui_edge_controls(
          hash,
          inputFn = selectDegree,
          prefix_input = "angle",
          label = "Angle",
          value = 0,
          width = "95%"
        )),
        # Edge Color
        col_4(ui_edge_controls(
          hash,
          inputFn = textInput,
          prefix_input = "color",
          label = "Edge",
          value = "black",
          width = "95%"
        )),
        # Curve Angle
        col_4(ui_edge_controls(
          hash,
          inputFn = selectInput,
          prefix_input = "lty",
          label = "Line Type",
          choices = c("solid", "dashed"),
          selected = "solid",
          width = "95%"
        )),
        # Curve Angle
        col_4(ui_edge_controls(
          hash,
          inputFn = selectInput,
          prefix_input = "lineT",
          label = "Line Thickness",
          choices = c("ultra thin", "very thin", "thin", "semithick", "thick", "very thick", "ultra thick"),
          selected = "thin",
          width = "95%"
        ))
      )
    )
  }
  
  # Create the edge aesthetics control UI, only updated when tab is activated
  output$edge_aes_ui <- renderUI({
    req(input$tab_control == "edit_edge_aesthetics")
    req(length(isolate(rve$edges)) > 0)
    rv_edge_frame <- edge_frame(isolate(rve$edges), isolate(rvn$nodes))
    
    tagList(
      purrr:::pmap(rv_edge_frame, ui_edge_controls_row)
    )
  })
  
  # Watch edge UI inputs and update rve$edges when inputs change
  observe({
    req(length(rve$edges) > 0, grepl("^angle_", names(input)))
    rv_edges <- rve$edges
    edge_ui <- tibble(
      inputId = grep("^(angle|color|lty|lineT)_", names(input), value = TRUE)
    ) %>%
      filter(!grepl("-selectized$", inputId)) %>%
      # get current value of input
      mutate(value = lapply(inputId, function(x) input[[x]])) %>%
      tidyr::separate(inputId, into = c("var", "hash"), sep = "_") %>%
      tidyr::spread(var, value) %>%
      tidyr::unnest() %>%
      split(.$hash)
    for (edge in edge_ui) {
      if (!edge$hash %in% names(rv_edges)) next
      this_edge <- edge[setdiff(names(edge), "hash")]
      for (prop in names(this_edge)) {
        rve$edges[[edge$hash]][[prop]] <- this_edge[[prop]]
      }
    }
    debug_input(rv_edges, "rve$edges after aes update")
  }, priority = -50)
  
  # ---- Render DAG ----
  output$tikzOut <- renderUI({
    req(length(rvn$nodes), input$showPreview)
    
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
  
  edge_points_rv <- reactive({
    req(length(rve$edges) > 0)
    ep <- edge_points(rve$edges, rvn$nodes)
    ep[complete.cases(ep), ]
  })
  
  dag_node_lines <- function(nodeFrame) {
    # Node frame is all points (1, 7) to (7, 1) with columns x, y, name
    nodeFrame <- nodeFrame[
      nodeFrame$x >= min(nodeFrame[!is.na(nodeFrame$name), ]$x) &
        nodeFrame$x <= max(nodeFrame[!is.na(nodeFrame$name), ]$x) &
        nodeFrame$y >= min(nodeFrame[!is.na(nodeFrame$name), ]$y) &
        nodeFrame$y <= max(nodeFrame[!is.na(nodeFrame$name), ]$y),
      ]
    nodeFrame$name <- ifelse(is.na(nodeFrame$name), "~", nodeFrame$name)
    nodeFrame$nameA <- nodeFrame$name
    idx_node_adjusted <- which(nodeFrame$hash %in% input$adjustNode)
    nodeFrame$nameA[idx_node_adjusted] <- as.character(
      glue::glue(" |[module]| {nodeFrame$nameA[idx_node_adjusted]}")
    )
    nodeLines <- vector("character", 0)
    for (i in unique(nodeFrame$y)) {
      createLines <- paste0(paste(nodeFrame[nodeFrame$y == i, ]$nameA, collapse = "&"), "\\\\")
      nodeLines <- c(nodeLines, createLines)
    }
    nodeLines <- rev(nodeLines)
    
    paste0("\\matrix(m)[matrix of nodes, row sep=2.6em, column sep=2.8em,text height=1.5ex, text depth=0.25ex, nodes={label}] {", paste(nodeLines, collapse = ""), "};")
  }
  
  tikz_code <- reactive({
    req(rvn$nodes, input$showPreview || input$tab_control == "edit_latex")
    nodeFrame <- node_frame(rvn$nodes)
    req(nrow(nodeFrame) > 0)
    
    styleZ <- "\\tikzset{ module/.style={draw, rectangle},
      label/.style={ } }"
    startZ <- "\\begin{tikzpicture}[>=latex]"
    endZ <- "\\end{tikzpicture}"
    pathZ <- "\\path[->,font=\\scriptsize,>=angle 90]"
    
    nodePts <- tidyr::crossing(x = 1:7, y = 1:7) %>%
      left_join(
        nodeFrame,
        by = c("x", "y")
      )
    
    x_min <- nodePts %>% filter(!is.na(hash)) %>% pull(x) %>% min()
    y_max <- nodePts %>% filter(!is.na(hash)) %>% pull(y) %>% max()
    
    nodeLines <- dag_node_lines(nodePts)
    
    edgeLines <- character()
    
    if (length(rve$edges)) {
      # edge_points_rv() is a reactive that gathers values from aesthetics UI
      # but it can be noisy, so we're debouncing to delay TeX rendering until values are constant
      edgePts <- debounce(edge_points_rv, 5000)()
      
      tikz_point <- function(x, y, x_min, y_max) {
        glue::glue("(m-{y_max - y + 1}-{x - x_min + 1})")
      }
      
      edgePts <- edgePts %>%
        mutate(
          parent = tikz_point(from.x, from.y, x_min, y_max),
          child = tikz_point(to.x, to.y, x_min, y_max),
          edgeLine = glue::glue(
            "{parent} edge [>={input$arrowShape}, bend left = {edgePts$angle}, ",
            "color = {edgePts$color},{edgePts$lineT},{edgePts$lty}] node[auto] {{$~$}} {child}"
          )
        )
      
      debug_input(select(edgePts, hash, matches("^(from|to)"), parent, child, edgeLine), "edgeLines")
      edgeLines <- edgePts$edgeLine
    }
    
    edgeLines <- paste0(pathZ, paste(edgeLines, collapse = ""), ";")
    
    paste(c(styleZ, startZ, nodeLines, edgeLines, endZ), collapse = "\n")
  })
  
  which_tex_preview <- reactive({
    has_main_app_code <- !is.null(tikz_code())
    on_manual_tikz_tab <- input$tab_control == "edit_latex"
    has_manual_tikz_init <- isTruthy(input$manual_tikz)
    
    if (!has_main_app_code && !on_manual_tikz_tab) {
      "none"
    } else if (on_manual_tikz_tab && has_manual_tikz_init) {
      "manual"
    } else {
      "app"
    }
  })
  
  has_touched_manual <- FALSE
  output$showPreview_helptext <- renderUI({
    tex_showing <- which_tex_preview()
    if (tex_showing == "manual") {
      has_touched_manual <<- TRUE
      helpText("Previewing manually entered TikZ TeX")
    } else if (has_touched_manual && tex_showing == "app") {
      helpText("Previewing shinyDAG TikZ TeX")
    }
  })
  
  tikz_cache_dir <- reactiveVal(NULL)
  
  # Re-render TeX preview
  observe({
    req(input$showPreview)
    
    tikz_lines <- switch(
      which_tex_preview(),
      "manual" = input$manual_tikz,
      "app" = tikz_code(),
      return(invisible())
    )
    
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
  
  make_graph <- function(nodes, edges) {
    g <- make_empty_graph()
    if (nrow(node_frame(nodes))) {
      g <- g + node_vertices(nodes)
    }
    if (length(edges)) {
      # Add edges
      g <- g + edge_edges(edges, nodes)
    }
    g
  }
  
  make_dagitty <- function(nodes, edges, exposure = NULL, outcome = NULL, adjusted = NULL) {
    dagitty_edges <- edge_frame(edges, nodes) %>% 
      glue::glue_data("{from} -> {to}") %>% 
      paste(collapse = "; ")
    
    gdag <- dagitty(glue::glue("dag {{ {dagitty_edges} }}"))
    
    if (isTruthy(exposure)) exposures(gdag) <- exposure
    if (isTruthy(outcome))  outcomes(gdag) <- outcome
    if (isTruthy(adjusted)) adjustedNodes(gdag) <- adjusted
    
    gdag
  }
  
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
        
      } else if (input$downloadType == "dagitty") {
        
        gdag <- make_dagitty(rvn$nodes, rve$edges, input$exposureNode, input$outcomeNode, input$adjustNode)
        
        saveRDS(gdag, file = file)
        
      } else if (input$downloadType == "ggdag") {
        
        tidy_dag <- 
          make_dagitty(rvn$nodes, rve$edges, input$exposureNode, input$outcomeNode, input$adjustNode) %>% 
          tidy_dagitty()
        
        saveRDS(tidy_dag, file = file)
      }
    },
    contentType = NA
  )
  
  # ---- TeX Editor ----
  output$texEdit <- renderUI({
    tikz_lines <- tikz_code()
    
    if (is.null(tikz_lines)) {
      tikz_lines <- "\\\\begin{tikzpicture}[>=latex]\n\\\\end{tikzpicture}"
    } else {
      # double escape backslashes
      tikz_lines <- gsub("\\", "\\\\", tikz_lines, fixed = TRUE)
    }
    aceEditor(
      "manual_tikz", 
      mode = "latex", 
      value = paste(tikz_lines, collapse = "\n"), 
      theme = "chrome",
      wordWrap = TRUE, 
      highlightActiveLine = TRUE
    )
  })

}
