
# * ui_edge_controls() builds an individual UI control element. These elements
#   are re-rendered whenever the tab is opened, so this function finds the
#   current value of the input and uses that instead of the value declared
#   in the definition in ui_edge_controls_row(). This function also isolates
#   the edge control UI from other changes in nodes, etc, because they happen
#   on different screens.
ui_controls <- function(hash, inputFn, prefix_input, label, ..., input = NULL) {
  stopifnot(!is.null(input))
  current_value_arg_name <- intersect(names(list(...)), c("selected", "value"))
  if (!length(current_value_arg_name)) {
    stop("Must specifiy `selected` or `value` when specifying edge UI controls")
  }
  input_name <- paste(prefix_input, hash, sep = "__")
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

get_hashed_input_with_prefix <- function(input, prefix, hash_sep = "__") {
  prefix <- glue::glue("^({prefix}){hash_sep}")

  tibble(
    inputId = grep(prefix, names(input), value = TRUE)
  ) %>%
    filter(!grepl("-selectized$", inputId)) %>%
    # get current value of input
    mutate(value = lapply(inputId, function(x) input[[x]])) %>%
    tidyr::separate(inputId, into = c("var", "hash"), sep = hash_sep) %>%
    tidyr::spread(var, value) %>%
    mutate_if(is.list, ~ purrr::map(.x, ~ if (is.null(.x)) NA else .x)) %>%
    tidyr::unnest() %>%
    split(.$hash)
}

# The input for angles (here for easy refactoring or future changes)
selectDegree <- function(inputId, label = "Degree", min = -180, max = 180, by = 15, value = 0, ...) {
  sliderInput(inputId, label = label, min = min, max = max, value = value, step = by)
}


# Edge Aesthetic UI -------------------------------------------------------

# These helper functions build up the Edge UI elements.
#
# * ui_edge_controls_row() creates the entire row of UI elements for a given
#   edge. This function is where the UI inputs are initially defined.

ui_edge_controls_row <- function(hash, from_name, to_name, ..., input = NULL) {
  stopifnot(!is.null(input))
  
  extra <- list(...)

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
      col_4(ui_controls(
        hash,
        inputFn = selectDegree,
        prefix_input = "angle",
        label = "Angle",
        value = extra[["angle"]] %||% 0,
        width = "95%",
        input = input
      )),
      # Edge Color
      col_4(ui_controls(
        hash,
        inputFn = xcolorPicker,
        prefix_input = "color",
        label = "Edge",
        selected = extra[["color"]] %||% "Black",
        width = "95%",
        input = input
      )),
      # Curve Angle
      col_4(ui_controls(
        hash,
        inputFn = selectInput,
        prefix_input = "lty",
        label = "Line Type",
        choices = c("solid", "dashed"),
        selected = extra[["lty"]] %||% "solid",
        width = "95%",
        input = input
      )),
      # Curve Angle
      col_4(ui_controls(
        hash,
        inputFn = selectInput,
        prefix_input = "lineT",
        label = "Line Thickness",
        choices = c("ultra thin", "very thin", "thin", "semithick", "thick", "very thick", "ultra thick"),
        selected = extra[["lineT"]] %||% "thin",
        width = "95%",
        input = input
      ))
    )
  )
}


# Node Aesthetic UI -------------------------------------------------------

ui_node_controls_row <- function(hash, name, adjusted, name_latex, ..., input = NULL) {
  stopifnot(!is.null(input))
  
  extra <- list(...)

  col_4 <- function(x) {
    tags$div(class = "col-sm-6 col-md-3", style = "min-height: 80px", x)
  }
  title_row <- function(x) tags$div(class = "col-xs-12", tags$h3(x))

  tagList(
    fluidRow(
      title_row(HTML(name))
    ),
    fluidRow(
      # LaTeX version of node label
      col_4(ui_controls(
        hash,
        inputFn = textInput,
        prefix_input = "name_latex",
        label = "LaTeX Label",
        value = name_latex,
        width = "95%",
        input = input
      )),
      # Text Color
      col_4(ui_controls(
        hash,
        inputFn = xcolorPicker,
        prefix_input = "color_text",
        label = "Text",
        selected = extra[["color_text"]] %||% "Black",
        width = "95%",
        input = input
      )),
      # Fill Color
      col_4(ui_controls(
        hash,
        inputFn = xcolorPicker,
        prefix_input = "color_fill",
        label = "Fill",
        selected = extra[["color_fill"]] %||% "White",
        width = "95%",
        input = input
      )),
      # Box Color (if shown)
      if (adjusted) {
        col_4(ui_controls(
          hash,
          inputFn = xcolorPicker,
          prefix_input = "color_draw",
          label = "Border",
          selected = extra[["color_draw"]] %||% "Black",
          width = "95%",
          input = input
        ))
      }
    )
  )
}
