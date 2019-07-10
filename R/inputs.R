# A fancy selectizeInput for angles
selectDegree <- function(
  inputId,
  label = "Degree",
  min = -180 + by,
  max = 180,
  by = 45,
  value = 0,
  ...
) {
  if (sign(min + (max - min)) != sign(by)) {
    by <- -by
  }
  choices <- seq(min, max, by)
  
  selectizeInput(inputId, label = label, choices, selected = value, multiple = FALSE, ..., )
}


# A button group that toggles state and optionally allows one button to be active at a time
buttonGroup <- function(inputId, options, btn_class = "btn-default", multiple = FALSE, aria_label = NULL) {
  btn_class <- paste("btn", paste(btn_class, collapse = " "))
  button_list <- purrr::imap(options, button_in_group, class = btn_class)
  selected <- shiny::restoreInput(inputId, default = "")
  tagList(
    singleton(tags$head(tags$script(src = "shinythingsButtonGroup.js"))),
    tags$div(
      class = "shinythings-btn-group btn-group",
      id = inputId,
      `data-input-id` = inputId,
      `data-active` = selected,
      `data-multiple` = as.integer(multiple),
      role = "group",
      button_list
    )
  )
}

button_in_group <- function(input_id, text, class = "btn btn-default") {
  tags$button(id = input_id, class = class, text)
}