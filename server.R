
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
  onBookmark(function(state) {
    state$values$rvn <- list()
    state$values$rvn$nodes <- rvn$nodes
    state$values$rve <- list()
    state$values$rve$edges <- rve$edges
    state$values$query_string <- session$clientData$url_search
    
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
    updateQueryString(url)
  })
  
  onRestore(function(state) {
    showModal(modalDialog(
      title = NULL,
      easyClose = FALSE,
      footer = NULL,
      tags$p(class = "text-center", "Loading your shinyDag workspace, please wait."),
      tags$div(class = "gerkelab-spinner")
    ))
    
    # clear selected node and text input to try to prevent existing values from
    # changing the name of the node that gets selected on restore
    rvn$nodes <- node_unset_attribute(rvn$nodes, names(rvn$nodes), "parent")
    updateTextInput(session, "node_list_node_name", value = "")
    
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
  
  # ---- Reactive Values Undo/Redo ----
  rv_undo_state <- shinyThings::undoHistory(
    id = "undo_rv", 
    value = reactive({
      req(length(rvn$nodes) > 0)
      list(
        nodes = rvn$nodes,
        edges = rve$edges
      )
    })
  )
  
  observe({
    req(!is.null(rv_undo_state()))
    
    rv_state <- rv_undo_state()
    debug_input(rv_state$nodes, "undo/redo - new nodes")
    debug_input(rv_state$edges, "undo/redo - new edges")
    rvn$nodes <- rv_state$nodes
    rve$edges <- rv_state$edges
  }, priority = 1000)

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
    node_to_delete <- node_list_selected_node()
    rvn$nodes[[node_to_delete]] <- NULL
    
    # Remove any edges
    edges_with_node <- rve$edges %>% 
      purrr::keep(~ node_to_delete %in% c(.$from, .$to)) %>% 
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
    debug_input(new, "clickpad_new_locations()")
    
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
      has_edge <- edge_exists(isolate(rve$edges), s_node_parent, s_node_child %||% clicked_hash)
      has_reverse_edge <- edge_exists(isolate(rve$edges), s_node_child %||% clicked_hash, s_node_parent)
      
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
      
      # Remove reverse edge if it exists
      rv_edges <- isolate(rve$edges)
      if (has_reverse_edge) {
        rv_edges <- edge_toggle(rv_edges, clicked_hash, s_node_parent)
      }
      rve$edges <- edge_toggle(rv_edges, s_node_parent, clicked_hash)
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
  make_dagitty <- function(nodes, edges, exposure = NULL, outcome = NULL, adjusted = NULL) {
    dagitty_edges <- edge_frame(edges, nodes) %>% 
      glue::glue_data("{from_name} -> {to_name}") %>% 
      paste(collapse = "; ")
    
    dagitty_code <- glue::glue("dag {{ {dagitty_edges} }}")
    debug_input(dagitty_code, "dagitty_code")
    
    gdag <- dagitty(dagitty_code)
    
    if (isTruthy(exposure)) exposures(gdag) <- node_name_from_hash(nodes, exposure)
    if (isTruthy(outcome))  outcomes(gdag) <- node_name_from_hash(nodes, outcome)
    if (isTruthy(adjusted)) adjustedNodes(gdag) <- node_name_from_hash(nodes, adjusted)
    
    gdag
  }
  
  dagitty_open_exp_outcome_paths <- reactive({
    req(
      length(nodes_in_dag(rvn$nodes)),
      length(edges_in_dag(rve$edges, rvn$nodes))
    )
    
    # need both exposure and outcome node
    requires_nodes <- c("Exposure" = input$exposureNode, "Outcome" = input$outcomeNode)
    missing_nodes <- names(requires_nodes[grepl("^$", requires_nodes)])
    validate(
      need(
        length(missing_nodes) == 0,
        glue::glue("Please choose {str_and(missing_nodes)} {str_plural(missing_nodes, 'node')}")
      )
    )
    
    purrr::safely(dagitty_open_paths)(
      nodes = rvn$nodes, edges = rve$edges, exposure = input$exposureNode, 
      outcome = input$outcomeNode, adjusted = input$adjustNode
    )
  })
  
  dagitty_open_paths <- function(nodes, edges, exposure, outcome, adjusted) {
    node_names <- invertNames(node_names(nodes))
    gd <- make_dagitty(
      edges = edges, nodes = nodes,
      exposure = exposure, outcome = outcome, adjusted = adjusted
    )
    
    exp_outcome_paths <- paths(
      gd,
      Z = adjusted %??% unname(node_names[adjusted])
    )
    
    exp_outcome_paths$paths[as.logical(exp_outcome_paths$open)]
  }
  
  dagitty_format_paths <- function(paths) {
    HTML(paste0(
      "<pre><code>",
      paste(trimws(paths), collapse = "
"),
      "\n</code></pre>"
    ))
  }
  
  output$openExpOutcomePaths <- renderUI({
    validate(need(length(edges_in_dag(rve$edges, rvn$nodes)) > 0, "Please add at least one edge"))
    
    open_paths <- dagitty_open_exp_outcome_paths()
    
    validate(need(
      is.null(open_paths$error),
      paste(
        "There was an error building your graph. It may not be fully or",
        "correctly specified."
      )
    ), errorClass = " text-danger")
    
    open_paths <- open_paths$result
    
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
  
  ui_edge_controls_row <- function(hash, from_name, to_name, ...) {
    col_4 <- function(x) {
      tags$div(class = "col-sm-6 col-md-3", style = "min-height: 80px", x)
    }
    title_row <- function(x) tags$div(class = "col-xs-12", tags$h3(x))
    edge_label <- paste0(from_name, "&nbsp;&#8594; ", to_name)
    
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
    rv_edges <- isolate(rve$edges)
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
        rv_edges[[edge$hash]][[prop]] <- this_edge[[prop]]
      }
    }
    debug_input(bind_rows(rv_edges, .id = "hash"), "rve$edges after aes update")
    rve$edges <- rv_edges
  }, priority = -50)
  
  # ---- Prepare DAG from App ----

  edge_points_rv <- reactive({
    req(length(rve$edges) > 0)
    ep <- edge_points(rve$edges, rvn$nodes)
    req(nrow(ep) > 0)
    ep
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
  
  tikz_code_from_app <- reactive({
    req(rvn$nodes, tweak_preview_visible())
    nodeFrame <- node_frame(rvn$nodes)
    req(nrow(nodeFrame) > 0)
    
    styleZ <- "\\tikzset{ module/.style={draw, rectangle},
      label/.style={ } }"
    startZ <- "\\begin{tikzpicture}[>=latex]"
    endZ <- "\\end{tikzpicture}"
    pathZ <- "\\path[->,font=\\scriptsize,>=angle 90]"
    
    nodePts <- node_frame(rvn$nodes)
    
    d_x <- min(nodePts$x) - 1L
    d_y <- min(nodePts$y) - 1L
  
    nodePts$x <- nodePts$x - d_x
    nodePts$y <- nodePts$y - d_y
    
    y_max <- max(nodePts$y)
    
    nodeLines <- nodePts %>% 
      tidyr::complete(
        x = seq(min(nodePts$x), max(nodePts$x)), 
        y = seq(min(nodePts$y), max(nodePts$y))
      ) %>% 
      dag_node_lines()
    
    edgeLines <- character()
    
    if (length(rve$edges)) {
      # edge_points_rv() is a reactive that gathers values from aesthetics UI
      # but it can be noisy, so we're debouncing to delay TeX rendering until values are constant
      edgePts <- debounce(edge_points_rv, 5000)()
      
      tikz_point <- function(x, y, d_x, d_y, y_max) {
        glue::glue("(m-{y_max - (y - d_y) + 1}-{x - d_x})")
      }
      
      edgePts <- edgePts %>%
        mutate(
          parent = tikz_point(from.x, from.y, d_x, d_y, y_max),
          child = tikz_point(to.x, to.y, d_x, d_y, y_max),
          edgeLine = glue::glue(
            "{parent} edge [>={input$arrowShape}, bend left = {edgePts$angle}, ",
            "color = {edgePts$color},{edgePts$lineT},{edgePts$lty}] node[auto] {{$~$}} {child}"
          )
        )
      
      debug_input(select(edgePts, hash, matches("^(from|to)_name"), parent, child, edgeLine), "edgeLines")
      edgeLines <- edgePts$edgeLine
    }
    
    edgeLines <- paste0(pathZ, paste(edgeLines, collapse = ""), ";")
    
    paste(c(styleZ, startZ, nodeLines, edgeLines, endZ), collapse = "\n")
  })
  
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

  dag_dagitty <- reactive({
    req(
      tweak_preview_visible(),
      length(nodes_in_dag(rvn$nodes)), 
      length(edges_in_dag(rve$edges)),
      input$exposureNode, input$outcomeNode, input$adjustNode
    )
    make_dagitty(rvn$nodes, rve$edges, input$exposureNode, input$outcomeNode, input$adjustNode)
  })
  
  dag_tidy <- reactive({
    req(
      tweak_preview_visible(),
      length(nodes_in_dag(rvn$nodes)), 
      length(edges_in_dag(rve$edges)),
      input$exposureNode, input$outcomeNode, input$adjustNode
    )
    make_dagitty(rvn$nodes, rve$edges, input$exposureNode, input$outcomeNode, input$adjustNode) %>% 
      tidy_dagitty()
  })
  
  # ---- App-based TikZ Preview ----
  tweak_preview_visible <- callModule(
    module = dagPreview,
    id = "tweak_preview",
    session_dir = SESSION_TEMPDIR,
    tikz_code = reactive({
      req(input$shinydag_page == "tweak")
      tikz_code_from_app()
    }),
    dag_dagitty,
    dag_tidy
  )
  
  # ---- TeX Editor ----
  output$texEdit <- renderUI({
    tikz_lines <- tikz_code_from_app()
    
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
  
  latex_preview_visible <- callModule(
    module = dagPreview,
    id = "latex_preview",
    session_dir = SESSION_TEMPDIR,
    reactive({
      req(input$shinydag_page == "latex")
      input$manual_tikz
    })
  )

}
