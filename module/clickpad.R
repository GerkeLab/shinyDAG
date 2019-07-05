clickpad_UI <- function(id, ...) {
  library(plotly)
  ns <- NS(id)
  tagList(
    plotlyOutput(ns("plot"), ...)
  )
}

clickpad_debug <- function(id, click = TRUE, doubleclick = TRUE, selected = FALSE, clickannotation = TRUE) {
  ns <- NS(id)
  col_width <- 12 / sum(click, doubleclick, selected, clickannotation)
  tagList(
    fluidRow(
      style = "overflow-y: scroll; max-height: 200px;",
      if (click) column(
        col_width, 
        tags$p(tags$code("plotly_click")),
        verbatimTextOutput(ns("v_click"))
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
  primary_node = NULL, 
  secondary_node = NULL, 
  plotly_source = "clickpad"
) {
  library(plotly)
  ns <- session$ns
  
  node_primary <- reactive({
    if (isTruthy(primary_node())) primary_node()
  })
  node_secondary <- reactive({
    if (isTruthy(secondary_node())) secondary_node()
  })
  
  output$v_click <- renderPrint({ 
    str(event_data("plotly_click", priority = "event", source = plotly_source))
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
    edge_points(edges(), nodes()) %>% 
      purrr::pmap_chr(arrow_path, dist = 0.2) %>% 
      purrr::map(~ list(
        type = "path",
        line = list(color = "#000", width = 1), 
        fillcolor = "#000",
        path = .x,
        opacity = 0.75
      ))
  })
  
  annotations <- reactive({
    if (is.null(nodes()) || length(nodes()) == 0) return(NULL)
    node_frame(nodes()) %>% 
      purrr::pmap(function(x, y, name, hash, ...) {
        border_color <- if (!is.null(node_primary()) && node_primary() == hash) {
          "#D3751C"
        } else if (!is.null(node_secondary()) && node_secondary() == hash) {
          "#161688"
        } else "#EEEEEE"
        
        list(text = name, 
             point_hash = hash, 
             x = x, 
             y = y, 
             font = list(size = 24),
             showarrow = FALSE, 
             align = "center",
             captureevents = TRUE,
             textposition = "middle center",
             bordercolor = border_color, 
             bgcolor = "#FFFFFF",
             borderpad = 4)
      })
  })
  
  output$plot <- renderPlotly({
    debug_line("rendering clickpad")
    redraw_plot()
    
    ax <- list(
      title = "",
      zeroline = FALSE,
      showline = FALSE,
      showticklabels = FALSE,
      showgrid = TRUE,
      # domain = list(0.5, 7.5),
      range = list(0.5, 7.5)
      # rangemode = "nonnegative",
      # fixedrange = TRUE
      # constrain = "range"
    )
    
    p <- plot_ly(type = "scatter", source = plotly_source)
    
    if (FALSE && !is.null(nodes()) && length(nodes()) > 0) {
      p <- p %>% 
      add_markers(
        data = node_frame(nodes()),
        x = ~x,
        y = ~y,
        # hash = ~hash,
        showlegend = FALSE,
        color = I("#FFFFFF")
      )
    }
    
    p %>% 
      layout(
        shapes = arrows(),
        annotations = annotations(), 
        xaxis = ax,
        yaxis = ax
        # yaxis = c(ax, list(scaleanchor = "x"))
      ) %>% 
      config(
        edits = list(
          # shapePosition = TRUE, 
          annotationPosition = TRUE
        ),
        showAxisDragHandles = FALSE,
        displayModeBar = FALSE
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
  
  new_locations <- reactive({
    req(annotations())
    ## https://stackoverflow.com/questions/54990350/extract-xyz-coordinates-from-draggable-shape-in-plotly-ternary-r-shiny
    
    event <- event_data("plotly_relayout", source = plotly_source)
    if (!length(event)) return()
    
    annot_event <- event[grepl("^annotations\\[\\d+\\]\\.[xy]$", names(event))]
    annot_index <- sub(".+\\[(\\d+)\\].+", "\\1", names(annot_event)[1]) %>% as.integer()
    
    if (!is.integer(annot_index)) return()
    # browser()
    
    point_hash <- annotations()[[annot_index + 1]]$point_hash
    # cli::cat_line("event_name: ", names(event)[1])
    # cli::cat_line("annot_index: ", annot_index)
    # cli::cat_line("point_hash: ", point_hash)
    
    req(!is.null(point_hash))
    
    new_x <- annot_event[grepl("\\.x", names(annot_event))] %>% 
      unlist() %>% mean() %>% round(0)
    new_y <- annot_event[grepl("\\.y", names(annot_event))] %>% 
      unlist() %>% mean() %>% round(0)
    
    # cli::cat_line("new_x: ", new_x)
    # cli::cat_line("new_y: ", new_y)
    
    list(
      hash = point_hash,
      x = new_x,
      y = new_y
    )
  })
  
  return(reactive(new_locations()))
}