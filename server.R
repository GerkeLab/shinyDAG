
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
    state$values$rv <- list()
    for (var in names(rv)) {
      state$values$rv[[var]] <- rv[[var]]
    }
    state$values$node_list_btn_last_state <- node_list_btn_last_state
    
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
      tags$p(class = "text-center", "Loading your ShinyDag workspace, please wait.")
    ))
    if (isTRUE(getOption("shinydag.debug", FALSE))) {
      names(state$values) %>%
        purrr::set_names() %>%
        purrr::map(~ state$values[[.]]) %>%
        purrr::compact() %>%
        purrr::iwalk(~ debug_input(.x, paste0("state$values$", .y)))
    }
    for (var in names(rv)) {
      rv[[var]] <- state$values$rv[[var]]
    }
    node_list_btn_last_state <<- state$values$node_list_btn_last_state
  })
  
  onRestored(function(state) {
    removeModal()
    updateSelectInput(session, "exposureNode", selected = state$values$sel$exposureNode)
    updateSelectInput(session, "outcomeNode", selected = state$values$sel$outcomeNode)
    updateSelectizeInput(session, "adjustNode", selected = state$values$sel$adjustNode)
  })
  
  # ---- Reactive Values ----
  rv <- reactiveValues(
    # g = make_empty_graph(),
    # gg = make_empty_graph(),
    edges = list(),
    nodes = list(),
    pts = list(x = vector("numeric", 0), y = vector("numeric", 0), name = vector("character", 0)),
    pts2 = tibble(x = rep(1:7, each = 7), y = rep(1:7, 7), name = rep(NA, 49))
  )
  
  node_list_btn_last_state <- c()
  
  # rv$edges is a named list, e.g. for hash(A) -> hash(B):
  # rv$edges[edge_key(hash(A), hash(B))] = list(from = hash(A), to = hash(B))
  
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
    } else {
      TRUE
    }
  }
  
  node_names <- function(nodes, all = FALSE) {
    if (!length(nodes)) {
      return(character())
    }
    x <- invertNames(sapply(nodes, function(x) x$name))
    if (all) {
      return(x)
    }
    has_position <- sapply(nodes, function(x) !is.na(x$x))
    x[has_position]
  }
  
  node_name_from_hash <- function(nodes, hash) {
    invertNames(node_names(isolate(rv$nodes), all = TRUE))[hash]
  }
  
  node_update <- function(nodes, hash, name = NULL, x = NULL, y = NULL) {
    nodes[[hash]]$name <- name %||% nodes[[hash]]$name
    nodes[[hash]]$x <- x %||% nodes[[hash]]$x
    nodes[[hash]]$y <- y %||% nodes[[hash]]$y
    nodes
  }
  
  node_delete <- function(nodes, hash) {
    .nodes <- nodes[setdiff(names(nodes), hash)]
    if (length(.nodes)) .nodes else list()
  }
  
  node_frame <- function(nodes, full = FALSE) {
    if (!length(nodes)) {
      return(tibble())
    }
    x <- bind_rows(nodes) %>%
      mutate(hash = names(nodes)) %>%
      select(hash, everything())
    if (full) {
      return(x)
    }
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
  
  nodes_in_dag <- function(nodes) {
    nodes %>%
      purrr::keep(~ !is.na(.$x)) %>%
      names()
  }
  
  # ---- Node Controls ----
  node_btn_id <- function(node_hash) paste0("node_toggle_", node_hash)
  node_btn_get_hash <- function(node_btn_id) sub("node_toggle_", "", node_btn_id, fixed = TRUE)
  
  output$nodeListButtonsLabel <- renderUI({
    if (!length(rv$nodes)) {
      tags$p(tags$strong("Add a Node"))
    } else if (is.null(input$node_list_selected_node)) {
      tags$p(tags$strong("Add Node or Select Existing to Edit or Place"))
    } else {
      tags$p(tags$strong("Edit or Place Selected Node"))
    }
  })
  
  
  output$nodeListButtons <- renderUI({
    req(rv$nodes)
    node_list_buttons_redraw()
    if (!length(rv$nodes)) {
      return()
    }
    
    s_node <- isolate(node_list_newest_node()) %||% input$node_list_selected_node
    
    buttonGroup(
      "node_list_selected_node",
      choices = node_names(rv$nodes, all = TRUE),
      multiple = FALSE,
      selected = s_node
    )
  })
  
  node_list_buttons_redraw <- reactiveVal(Sys.time())
  node_list_newest_node <- reactiveVal(NULL)
  
  # Handle add node button, creates new node and sets focus
  observeEvent(input$node_list_node_add, {
    new_node_hash <- digest::digest(Sys.time())
    rv$nodes <- node_new(rv$nodes, new_node_hash, "new node")
    node_list_buttons_redraw(Sys.time())
    node_list_newest_node(new_node_hash)
    updateTextInput(session, "node_list_node_name", value = "", placeholder = "Enter Node Name")
    shinyjs::show("node_list_node_name_container")
    shinyjs::runjs("set_input_focus('node_list_node_name')")
  })
  
  observeEvent(input$node_list_selected_node, {
    if (is.null(input$node_list_selected_node)) {
      shinyjs::hide("node_list_node_name_container")
      return()
    } 
    
    if (!is.null(node_list_newest_node()) && 
        node_list_newest_node() == input$node_list_selected_node) {
      # A new node was recently selected, and was handled above
      return()
    }
    
    # Selected node already exists, update UI
    node_list_newest_node(NULL)
    shinyjs::show("node_list_node_name_container")
    s_node_name <- node_name_from_hash(isolate(rv$nodes), input$node_list_selected_node)
    updateTextInput(
      session, 
      "node_list_node_name", 
      value = unname(s_node_name)
    )
  }, priority = 1000)
  
  # Name of currently selected node
  # node_list_name <- reactive({
  #   req(input$node_list_node_name, input$node_list_selected_node)
  #   input$node_list_node_name
  # })
  
  # Handle node name text input
  observeEvent(input$node_list_node_name, {
    # node_name_debounced <- debounce(node_list_name, 500)
    # debug_input(node_label_debounced(), "node_label_debounced")
    req(input$node_list_selected_node, input$node_list_node_name != "")
    rv$nodes <- node_update(rv$nodes, input$node_list_selected_node, input$node_list_node_name)
  }, priority = -1000)
  
  # Show editing buttons when appropriate
  observe({
    I("toggle edit buttons")
    if (is.null(input$node_list_selected_node)) {
      # no node selected, can only add a new node
      shinyjs::hide("node_list_node_remove")
      shinyjs::hide("node_list_node_delete")
    } else {
      if (input$node_list_selected_node %in% nodes_in_dag(rv$nodes)) {
        # if the node is in the DAG it can be removed
        shinyjs::show("node_list_node_remove")
        shinyjs::hide("node_list_node_delete")
      } else {
        # if it's not in the DAG it can be deleted
        shinyjs::hide("node_list_node_remove")
        shinyjs::show("node_list_node_delete")
      }
    }
  })
  
  # Action: remove node from DAG
  observeEvent(input$node_list_node_remove, {
    rv$nodes <- node_update(rv$nodes, input$node_list_selected_node, x = NA, y = NA)
  })
  
  # Action: delete node
  observeEvent(input$node_list_node_delete, {
    # Remove node
    rv$nodes[[input$node_list_selected_node]] <- NULL
    
    # Remove any edges
    edges_with_node <- rv$edges %>% 
      purrr::keep(~ input$node_list_selected_node %in% c(.$from, .$to)) %>% 
      names()
    
    if (length(edges_with_node)) rv$edges[edges_with_node] <- NULL
    
    shinyjs::hide("node_list_node_name_container")
  })
  
  # ---- Click Pad ----
  req_nodes <- function() {
    if (!length(rv$nodes)) {
      warnNotification("Please add a node")
      FALSE
    }
    TRUE
  }
  
  # Click Behavior Overview
  # 1. Double Click to Select/Deselect Node
  #     - Sets as Parent Node
  # 2. Single Click With Active Node
  #     - Blank Space: move/place node
  #     - On second node: set Child Node
  
  toggle_node_btn_state <- function(id, state = NULL) {
    if (!grepl("_", id)) {
      # If no underscore, then it was a hash and not the full id
      id <- node_btn_id(id)
    }
    
    state <- state %||% !input[[id]]
    updateButton(session, id, value = state)
  }
  
  # Single Click Handler
  observeEvent(input$pad_click, {
    if (!req_nodes()) {
      return()
    }
    
    if (is.null(input$node_list_selected_node)) {
      # No active node
      warnNotification("Please double click to select a node")
      return()
    }
    
    # Single Click on Node: Set Child Node
    nearest_node <- node_nearest(rv$nodes, input$pad_click)
    if (nrow(nearest_node)) {
      updateSelectizeInput(session, "to_edge", selected = nearest_node$hash)
      return()
    }
    
    # Single Click on Space: Move Active Node
    node_hash <- node_btn_get_hash(input$node_list_selected_node)
    
    if (isTRUE(is.na(rv$nodes[[node_hash]]$x))) {
      # Set flag for edge UI updater to make this node the parent node
      rv$nodes[[node_hash]]$new_to_dag <- TRUE
    }
    
    rv$nodes <- node_update(
      rv$nodes,
      node_hash,
      x = round(input$pad_click$x),
      y = round(input$pad_click$y)
    )
    
    debug_input(rv$nodes, "rv$nodes")
  })
  
  # Double Click Handler
  observeEvent(input$pad_dblclick, {
    if (!req_nodes()) {
      return()
    }
    
    nearest_node <- node_nearest(rv$nodes, input$pad_dblclick)
    if (is.null(input$node_list_selected_node)) {
      # No node currently active
      if (nrow(nearest_node)) {
        # Activate Node
        updateButtonGroupValue("node_list_selected_node", nearest_node$hash)
        # Set parent node
        updateSelectizeInput(session, "from_edge", selected = nearest_node$hash)
      }
    } else if (nrow(nearest_node)) {
      # Another node is already active
      s_node_btn <- input$node_list_selected_node
      toggle_node_btn_state(s_node_btn, state = FALSE)
      if (!identical(nearest_node$hash, s_node_btn %>% node_btn_get_hash())) {
        # Activate New Node
        updateButtonGroupValue("node_list_selected_node", nearest_node$hash)
        updateSelectizeInput(session, "from_edge", selected = nearest_node$hash)
      }
    }
  })
  
  # clickPad display
  output$clickPad <- renderPlot({
    req(rv$nodes)
    rv_pts <- node_frame(rv$nodes)
    
    if (nrow(rv_pts)) {
      active_node <- node_btn_get_hash(input$node_list_selected_node)
      if (!length(active_node)) active_node <- ""
      rv_pts <- mutate(
        rv_pts,
        color = case_when(
          # hash == active_node & hash == input$from_edge ~ "purple3",
          # hash == active_node & hash == input$to_edge ~ "darkorange3",
          hash == active_node ~ "firebrick3",
          # hash == input$from_edge ~ "steelblue3",
          # hash == input$to_edge ~ "goldenrod3",
          TRUE ~ "black"
        )
      )
      plot(rv_pts$x, rv_pts$y, xlim = c(1, 7), ylim = c(1, 7), bty = "n", xaxt = "n", yaxt = "n", ylab = "", xlab = "", xaxs = "i", col = "white")
      grid()
      # highlight from/to edge nodes
      if (input$from_edge != "") {
        edge_from_node <- rv_pts %>% filter(hash == input$from_edge)
        with(edge_from_node, points(x, y, bg = "grey94", cex = 12, pch = 22, col = NA))
      }
      if (input$to_edge != "") {
        edge_to_node <- rv_pts %>% filter(hash == input$to_edge)
        with(edge_to_node, points(x, y, bg = "grey94", cex = 12, pch = 21, col = NA))
      }
      if (!is.null(input$adjustNode) && input$adjustNode != "") {
        pts_adjust_node <- rv_pts %>% filter(hash %in% input$adjustNode)
        for (i in seq_along(pts_adjust_node)) {
          with(pts_adjust_node[i, ], points(x, y, col = "grey25", cex = 12, pch = 22))
        }
      }
      # add arrows
      if (length(rv$edges)) {
        e_pts <- edge_points(rv$edges, rv$nodes, push_by = 0.05)
        for (i in seq_len(nrow(e_pts))) {
          arrows(
            e_pts$from.x[i],
            e_pts$from.y[i],
            e_pts$to.x[i],
            e_pts$to.y[i],
            col = e_pts$color[i],
            lty = e_pts$lty[i],
            length = 0.1
          )
        }
      }
      # add text labels
      text(rv_pts$x, rv_pts$y, labels = rv_pts$name, cex = 2, col = rv_pts$color)
    } else {
      plot(NA, NA, xlim = c(1, 7), ylim = c(1, 7), bty = "n", xaxt = "n", yaxt = "n", ylab = "", xlab = "", xaxs = "i")
      grid()
    }
  }, height = 600)
  
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
    update_node_options(rv$nodes, "adjustNode", updateSelectizeInput, toggle = TRUE)
    update_node_options(rv$nodes, "exposureNode", updateSelectInput, toggle = TRUE)
    update_node_options(rv$nodes, "outcomeNode", updateSelectInput, toggle = TRUE)
  })
  
  observeEvent(input$exposureNode, {
    req(input$exposureNode)
    if (input$exposureNode == input$outcomeNode) {
      updateSelectInput(session, "outcomeNode", selected = "")
    }
  })
  
  observeEvent(input$outcomeNode, {
    req(input$outcomeNode)
    if (input$outcomeNode == input$exposureNode) {
      updateSelectInput(session, "exposureNode", selected = "")
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
  
  # ---- Edges - Add/Remove ----
  edge_key <- function(x, y) digest::digest(c(x, y))
  
  edge_frame <- function(edges, nodes, ...) {
    dots <- rlang::enexprs(...)
    
    all_edges <- bind_rows(edges) %>%
      mutate(hash = names(edges)) %>%
      tidyr::gather(position, node_hash, from:to)
    
    all_edges %>%
      filter(hash %in% edges_in_dag(edges, nodes)) %>%
      left_join(
        select(node_frame(nodes), hash, name),
        by = c("node_hash" = "hash")
      ) %>%
      tidyr::gather(var, value, node_hash, name) %>%
      mutate(
        var = sub("node|name", "", var),
        var = paste0(position, var)
      ) %>%
      select(-position) %>%
      tidyr::spread(var, value) %>%
      mutate(!!!dots)
  }
  
  edges_in_dag <- function(edges, nodes) {
    all_edges <- bind_rows(edges) %>%
      mutate(hash = names(edges)) %>%
      tidyr::gather(position, node_hash, from:to)
    
    edges_not_in_graph <- all_edges %>%
      filter(!node_hash %in% nodes_in_dag(nodes))
    
    setdiff(all_edges$hash, edges_not_in_graph$hash)
  }
  
  edge_edges <- function(edges, nodes, ...) {
    do.call(edge, as.list(edge_frame(edges, nodes, ...)))
  }
  
  edge_points <- function(edges, nodes, push_by = 0) {
    e_df <- edge_frame(edges, nodes)
    n_df <- node_frame(nodes)
    
    e_df %>% 
      left_join(
        n_df %>% purrr::set_names(paste0("from_", names(n_df))), 
        by = "from_hash"
      ) %>% 
      left_join(
        n_df %>% purrr::set_names(paste0("to_", names(n_df))), 
        by = "to_hash"
      ) %>%
      # TODO refactor rest of app to use from_x, etc. and remove line below
      rename(from.x = from_x, from.y = from_y, to.x = to_x, to.y = to_y) %>% 
      mutate(
        d_x = to.x - from.x,
        d_y = to.y - from.y,
        from.x = from.x + push_by * d_x,
        from.y = from.y + push_by * d_y,
        to.x = to.x - push_by * d_x,
        to.y = to.y - push_by * d_y
      )
  }
  
  node_choices <- reactiveVal(NULL)
  
  observe({
    # This observer exists to isolate parent/child node selection from
    # spurious changes in rv$nodes, i.e. to ensure that changes are propagated
    # only when the node name choices change
    choices <- node_names(rv$nodes)
    debug_input(choices, "choices")
    if (!identical(isolate(node_choices()), choices)) {
      node_choices(choices)
    }
  })
  
  # Update Parent/Child node selection for edges
  observeEvent(node_choices(), {
    req(node_choices())
    if (length(node_choices())) {
      
      # Force new node to be selected as parent node
      nodes_is_new <- purrr::map_lgl(rv$nodes, ~ "new_to_dag" %in% names(.))
      if (any(nodes_is_new)) {
        from_edge <- names(nodes_is_new[nodes_is_new])
        rv$nodes[[from_edge]]$new_to_dag <- NULL # remove flag
      } else {
        from_edge <- input$from_edge
      }
      
      updateSelectizeInput(
        session,
        "from_edge",
        choices = c("Choose edge parent" = "", node_choices()),
        selected = from_edge
      )
      updateSelectizeInput(
        session,
        "to_edge",
        choices = c("Choose edge child" = "", node_choices()),
        selected = input$to_edge
      )
    } else {
      node_choices <- c("Add a node to the plot area" = "")
      updateSelectizeInput(session, "from_edge", choices = node_choices())
      updateSelectizeInput(session, "to_edge", choices = node_choices())
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
        from = input$from_edge,
        to = input$to_edge,
        color = "black",
        angle = 0L,
        lineT = "thin",
        lty = "solid"
      )
    }
    debug_input(rv$edges, "rv$edges")
  })
  
  output$ui_edge_btn <- renderUI({
    if (is.null(input$from_edge) || is.null(input$to_edge)) {
      return()
    }
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
  
  observeEvent(input$ui_edge_swap_btn, {
    s_from_edge <- input$from_edge
    s_to_edge <- input$to_edge
    updateSelectizeInput(session, "from_edge", selected = s_to_edge)
    updateSelectizeInput(session, "to_edge", selected = s_from_edge)
  })
  
  observe({
    req(input$from_edge, input$to_edge)
    s_edges <- c(input$from_edge, input$to_edge)
    has_one_edge <- !all(s_edges == "")
    shinyjs::toggleState("ui_edge_swap_btn", has_one_edge)
  })
  
  # ---- DAG Diagnostics ----
  g_dagitty <- reactive({
    req(length(rv$edges) > 0)
    edges <- edge_frame(rv$edges, rv$nodes)
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
    req(rv$edges)
    
    # need both exposure and outcome node
    requires_nodes <- c("Exposure" = input$exposureNode, "Outcome" = input$outcomeNode)
    missing_nodes <- names(requires_nodes[grepl("^$", requires_nodes)])
    validate(
      need(
        length(missing_nodes) == 0,
        glue::glue("Please choose {str_and(missing_nodes)} {str_plural(missing_nodes, 'node')}")
      )
    )
    
    nodes <- invertNames(node_names(rv$nodes))
    gd <- g_dagitty() %>%
      dagitty_apply(
        rv$nodes,
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
    validate(need(length(rv$edges) > 0, "Please add at least one edge"))
    
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
  
  # A fancy selectizeInput for angles
  selectDegree <- function(inputId, label = "Degree", min = -180 + by, max = 180, by = 45, value = 0, ...) {
    if (sign(min + (max - min)) != sign(by)) {
      by <- -by
    }
    choices <- seq(min, max, by)
    
    selectizeInput(inputId, label = label, choices, selected = value, multiple = FALSE, ..., )
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
    req(length(isolate(rv$edges)) > 0)
    rv_edge_frame <- edge_frame(isolate(rv$edges), isolate(rv$nodes))
    
    tagList(
      purrr:::pmap(rv_edge_frame, ui_edge_controls_row)
    )
  })
  
  # Watch edge UI inputs and update rv$edges when inputs change
  observe({
    req(length(rv$edges) > 0, grepl("^angle_", names(input)))
    rv_edges <- rv$edges
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
        rv$edges[[edge$hash]][[prop]] <- this_edge[[prop]]
      }
    }
    debug_input(rv_edges, "rv$edges after aes update")
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
      style = "max-width: 100%; max-height: 600px; -o-object-fit: contain;",
      alt = "DAG"
    )
  })
  
  tikzUpdateOutput <- reactiveVal(TRUE) # Triggers PDF update when value changes
  
  edge_points_rv <- reactive({
    req(length(rv$edges) > 0)
    ep <- edge_points(rv$edges, rv$nodes)
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
          nodeFrame,
          by = c("x", "y")
        ) %>%
        dag_node_lines()
      
      edgeLines <- character()
      
      if (length(rv$edges)) {
        # edge_points_rv() is a reactive that gathers values from aesthetics UI
        # but it can be noisy, so we're debouncing to delay TeX rendering until values are constant
        edgePts <- debounce(edge_points_rv, 5000)()
        
        tikz_point <- function(x, y, x_min, y_max) {
          glue::glue("(m-{y_max - y + 1}-{x - x_min + 1})")
        }
        
        edgePts <- edgePts %>%
          mutate(
            x_min = min(from.x, to.x),
            y_max = max(from.y, to.y),
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
      paste0(
        "DAG",
        Sys.Date(),
        ifelse(
          input$downloadType == 1,
          ".RData",
          ifelse(
            input$downloadType == 2,
            ".tex",
            ifelse(
              input$downloadType == 3,
              ".png",
              ifelse(input$downloadType == 5, ".RData", ".pdf")
            )
          )
        )
      )
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
    },
    contentType = NA
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
      nodeFrame <- nodeFrame[
        nodeFrame$x >= min(nodeFrame[!is.na(nodeFrame$name), ]$x) &
          nodeFrame$x <= max(nodeFrame[!is.na(nodeFrame$name), ]$x) &
          nodeFrame$y >= min(nodeFrame[!is.na(nodeFrame$name), ]$y) &
          nodeFrame$y <= max(nodeFrame[!is.na(nodeFrame$name), ]$y),
        ]
      nodeFrame$name <- ifelse(is.na(nodeFrame$name), "~", nodeFrame$name)
      nodeFrame$nameA <- nodeFrame$name
      idx_node_adjusted <- which(nodeFrame$name %in% node_name_from_hash(input$adjustNode))
      nodeFrame$nameA[idx_node_adjusted] <- as.character(
        glue::glue(" |[module]| {nodeFrame$nameA[idx_node_adjusted]}")
      )
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
            "(m-",
            (nodeFrame[nodeFrame$name == edgeFrame$V1[i], ]$revY - min(nodeFrame$revY) + 1),
            "-",
            (nodeFrame[nodeFrame$name == edgeFrame$V1[i], ]$x - min(nodeFrame$x) + 1),
            ")"
          )
          edgeFrame$child[i] <- paste0(
            "(m-",
            (nodeFrame[nodeFrame$name == edgeFrame$V2[i], ]$revY - min(nodeFrame$revY) + 1),
            "-",
            (nodeFrame[nodeFrame$name == edgeFrame$V2[i], ]$x - min(nodeFrame$x) + 1),
            ")"
          )
          createEdge <- paste0(
            edgeFrame$parent[i],
            " edge [>=",
            input$arrowShape,
            ", bend left = ",
            edgeFrame$angle[i],
            ", color = ",
            edgeFrame$color[i],
            ",",
            edgeFrame$type[i],
            ",",
            edgeFrame$thick[i],
            "] node[auto] {$~$} ",
            edgeFrame$child[i],
            " "
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
      style = "height:560px; width:100%",
      src = file.path(session_id, "DAGimageEditDoc.pdf"),
      scrolling = "no",
      seamless = "seamless"
    ))
  })
  
  output$downloadButton2 <- downloadHandler(
    filename = function() {
      paste0(
        "DAG",
        Sys.Date(),
        ifelse(
          input$downloadType2 == 1,
          ".tex",
          ifelse(input$downloadType2 == 2, ".png", ".pdf")
        )
      )
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
    },
    contentType = NA
  )
}
