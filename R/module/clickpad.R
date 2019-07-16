clickpad_UI <- function(id, ...) {
  library(plotly)
  ns <- NS(id)
  tagList(
    plotlyOutput(ns("plot"), ...)
  )
}

clickpad_debug <- function(id, relayout = TRUE, doubleclick = TRUE, selected = FALSE, clickannotation = TRUE) {
  ns <- NS(id)
  col_width <- 12 / sum(relayout, doubleclick, selected, clickannotation)
  tagList(
    fluidRow(
      style = "overflow-y: scroll; max-height: 200px;",
      if (relayout) column(
        col_width, 
        tags$p(tags$code("plotly_relayout")),
        verbatimTextOutput(ns("v_relayout"))
      ),
      if (doubleclick) column(
        col_width,
        tags$p(tags$code("plotly_doubleclick")),
        verbatimTextOutput(ns("v_doubleclick"))
      ),
      if (selected) column(
        col_width,
        tags$p(tags$code("plotly_selected")),
        verbatimTextOutput(ns("v_selected"))
      ),
      if (clickannotation) column(
        col_width,
        tags$p(tags$code("plotly_clickannotation")),
        verbatimTextOutput(ns("v_clickannotation"))
      )
    )
  )
}

clickpad <- function(
  input, output, session, 
  nodes, edges, 
  plotly_source = "clickpad"
) {
  library(plotly)
  ns <- session$ns
  
  node_primary <- reactive({ node_parent(nodes()) })
  node_secondary <- reactive({ node_child(nodes()) })
  node_is_adjusted <- reactive({ node_adjusted(nodes()) })
  node_exposure <- reactive({ names(node_with_attribute(nodes(), "exposure")) })
  node_outcome <- reactive({ names(node_with_attribute(nodes(), "outcome")) })
  
  output$v_relayout <- renderPrint({ 
    str(event_data("plotly_relayout", priority = "event", source = plotly_source))
  })
  
  output$v_doubleclick <- renderPrint({ 
    str(event_data("plotly_doubleclick", priority = "event", source = plotly_source))
  })
  
  output$v_selected <- renderPrint({ 
    str(event_data("plotly_clickannotation", priority = "event", source = plotly_source))
  })
  
  output$v_clickannotation <- renderPrint({ 
    str(event_data("plotly_clickannotation", priority = "event", source = plotly_source))
  })
  
  arrow_path <- function(from.x, from.y, to.x, to.y, dist = 0.2, ...) {
    # angle of the line between `from` and `to`
    theta  <- atan2(to.y - from.y, to.x - from.x)
    
    # push line starting/ending points away from node by a fixed distance
    path_points = list(
      x0 = from.x + dist * cos(theta), 
      y0 = from.y + dist * sin(theta),
      x1 = to.x   - dist * cos(theta), 
      y1 = to.y   - dist * sin(theta)
    )
    
    # Find points for corners of arrow head (third point is `to`)
    arrow_anchor_x = path_points$x1 - dist * cos(theta)
    arrow_anchor_y = path_points$y1 - dist * sin(theta)
    ad <- 0.1 * dist / tan(1/6 * pi)
    
    path_points$a1_x = arrow_anchor_x + ad * cos(theta + 1/2 * pi)
    path_points$a1_y = arrow_anchor_y + ad * sin(theta + 1/2 * pi)
    path_points$a2_x = arrow_anchor_x - ad * cos(theta + 1/2 * pi)
    path_points$a2_y = arrow_anchor_y - ad * sin(theta + 1/2 * pi)
    
    # Draw arrow head in SVG path notation
    as.character(glue::glue_data(
      path_points,
      "M{x0},{y0} L{x1},{y1} L{a1_x},{a1_y} L{a2_x},{a2_y} L{x1},{y1}"
    ))
  }
  
  arrows <- reactive({
    if (is.null(edges()) || length(edges()) == 0) return(NULL)
    if (is.null(nodes()) || length(nodes()) == 0) return(NULL)
    
    ep <- edge_points(edges(), nodes())
    if (!nrow(ep)) return(NULL)
    
    ep %>% 
      purrr::pmap_chr(arrow_path, dist = 0.2) %>% 
      purrr::map(~ list(
        type = "path",
        line = list(color = "#000", width = 1), 
        fillcolor = "#000",
        path = .x,
        opacity = 0.75
      ))
  })
  
  create_node_annotations <- function(x, y, name, hash, ...) {
    set_color <- function(
      default, not_in_dag = NULL, 
      primary = NULL, secondary = NULL, 
      exposure = NULL, outcome = NULL, adjusted = NULL,
      apply_order = c("primary", "not_in_dag", "adjusted", "exposure", "outcome", "secondary")
    ) {
      applicable_states <- c(
        "primary" = !is.null(node_primary()) && hash %in% node_primary(),
        "secondary" = !is.null(node_secondary()) && hash %in% node_secondary(),
        "adjusted" = !is.null(node_is_adjusted()) && hash %in% node_is_adjusted(),
        "exposure" = !is.null(node_exposure()) && hash %in% node_exposure(),
        "outcome" = !is.null(node_outcome()) && hash %in% node_outcome(),
        "not_in_dag" = x < 0
      )
      
      applicable_states <- applicable_states[applicable_states]
      if (!length(applicable_states)) return(default)
      
      applicable_states <- applicable_states[apply_order]
      applicable_states <- applicable_states[!is.na(applicable_states)]
      if (!length(applicable_states)) return(default)
      
      color <- switch(
        names(applicable_states)[1],
        primary = primary,
        secondary = secondary,
        adjusted = adjusted,
        outcome = outcome,
        exposure = exposure,
        not_in_dag = not_in_dag,
        default
      )
      
      color %||% default
    }
    
    background_color <- set_color(
      default = "#FFFFFF", 
      not_in_dag = "#FDFDFD", 
      primary = "#F6E3D1"
    )
    font_color <- set_color(
      default = "#000000", 
      not_in_dag = "#666666", 
      primary = "#D3751C", 
      exposure = "#418c7a", 
      outcome = "#ba2d0b",
      apply_order = c("not_in_dag", "exposure", "outcome", "primary")
    )
    border_color <- set_color(
      default = "#EDEDED", 
      not_in_dag = "#AAAAAA", 
      primary = list(NULL), 
      adjusted = "#1c2d3f",
      apply_order = c("not_in_dag", "adjusted", "primary")
    )
      
    list(text = name, 
         node_hash = hash, 
         x = x, 
         y = y, 
         font = list(size = 24, color = font_color),
         showarrow = FALSE, 
         align = "center",
         captureevents = TRUE,
         textposition = "middle center",
         bordercolor = border_color,
         bgcolor = background_color,
         borderpad = 4)
  }
  
  annotations <- reactive({
    if (is.null(nodes()) || length(nodes()) == 0) return(NULL)
    node_frame(nodes(), full = TRUE) %>% 
      purrr::pmap(create_node_annotations)
  })
  
  left_margin <- list(
    type = "rect",
    line = list(color = "#AAAAAA", width = 1),
    fillcolor = "#EEEEEE",
    x0 = -100,
    y0 = -100,
    x1 = 0,
    y1 = 100
  )
  
  output$plot <- renderPlotly({
    debug_line("rendering clickpad")
    redraw_plot()
    
    ax <- list(
      title = "",
      zeroline = FALSE,
      showline = FALSE,
      showticklabels = FALSE,
      showgrid = TRUE,
      range = list(-1.5, 12.5)
    )
    ay <- ax
    y_min <- purrr::map_dbl(nodes(), "y") %>% min()
    y_max <- purrr::map_dbl(nodes(), "y") %>% max()
    ay$range <- list(min(0.5, y_min), max(7.5, y_max))
    
    p <- plot_ly(type = "scatter", source = plotly_source)
    
    p %>% 
      layout(
        annotations = annotations(), 
        shapes = c(list(left_margin), arrows()),
        xaxis = ax,
        yaxis = ay
      ) %>% 
      config(
        edits = list(
          annotationPosition = TRUE
        ),
        showAxisDragHandles = FALSE
        # displayModeBar = FALSE
      ) %>% 
      plotly::event_register("plotly_click") %>% 
      plotly::event_register("plotly_doubleclick") %>% 
      plotly::event_register("plotly_selected") %>% 
      plotly::event_register("plotly_clickannotation") %>% 
      htmlwidgets::onRender("
        function(el) { 
          el.on('plotly_hover', function(d) { console.log('Hover: ', d) });
          el.on('plotly_click', function(d) { console.log('Click: ', d) });
          el.on('plotly_selected', function(d) { console.log('Select: ', d) });
        }
      ")
  })
  
  redraw_plot <- reactiveVal(Sys.time())
  
  new_coords_lag <- list(hash = NA_character_, x = NA_real_, y = NA_real_)
  
  new_locations <- reactive({
    req(annotations())
    ## https://stackoverflow.com/questions/54990350/extract-xyz-coordinates-from-draggable-shape-in-plotly-ternary-r-shiny
    
    event <- event_data("plotly_relayout", source = plotly_source)
    if (!length(event)) return()
    
    annot_event <- event[grepl("^annotations\\[\\d+\\]\\.[xy]$", names(event))]
    annot_index <- sub(".+\\[(\\d+)\\].+", "\\1", names(annot_event)[1]) %>% as.integer()
    
    if (is.na(annot_index) || !is.integer(annot_index)) return()
    
    if (length(annotations()) <= annot_index) {
      stop("An error occurred, unable to match plotly update to correct node")
    }
    
    node_hash <- annotations()[[annot_index + 1]]$node_hash
    # cli::cat_line("event_name: ", names(event)[1])
    # cli::cat_line("annot_index: ", annot_index)
    # cli::cat_line("node_hash: ", node_hash)
    
    req(!is.null(node_hash))
    
    new_x <- annot_event[grepl("\\.x", names(annot_event))] %>% unlist() %>% unname()
    new_x <- if (new_x > 0) round(new_x, 0) else new_x
    
    new_y <- annot_event[grepl("\\.y", names(annot_event))] %>% unlist() %>% unname()
    new_y <- if (new_x > 0) round(new_y, 0) else new_y
    
    i_nodes <- isolate(nodes())
    current_pos <- i_nodes[[node_hash]][c("x", "y")] %>% unlist() %>% unname()
    
    new_coords <- list(
      hash = node_hash,
      x = new_x,
      y = new_y
    )

    if (identical(c(new_coords$x, new_coords$y), current_pos)) {
      # No change in current node position
      return()
    }
    
    if (identical(new_coords, new_coords_lag)) {
      # The plotly_redraw may not have been a result of annotation position change
      return()
    }
    
    new_coords_lag <<- new_coords
    
    # cli::cat_line("new_x: ", new_x)
    # cli::cat_line("new_y: ", new_y)
    new_coords
  })
  
  return(reactive(new_locations()))
}
